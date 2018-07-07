(ns factui.impl.store
  "A datom store. For storing datoms."
  (:require #?(:cljs [factui.facts :as f :refer [Datom]]
               :clj [factui.facts :as f])
            #?(:cljs [cljs.core :as c]
               :clj [clojure.core :as c])
           [factui.impl.txdata :as txdata])
  (:refer-clojure :exclude [update resolve])
  #?(:clj (:import [factui.facts Datom])))

(defprotocol Store
  (update [this insert-datoms retract-datoms]
    "Update the store by inserting and retracting the specified concrete datoms (such as those returned by `resolve`).")
  (resolve [this operations]
    "Given a seq of Datomid operations, resolve against the existing contents of the store to return a 3-tuple of:

    1. concrete tuples to add
    2. concrete tuples to remove
    3. map of tempid to concrete EIDs

    This handles all Datomic-style semantics such as upsert, tempid resolution, and preventing duplicates")
  (schema [this] "Returns a map of {attr attributes} for the store's schema."))

(def ^:no-doc base-schema
  "Initial built-in schema"
  [{:db/ident :db/valueType
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/unique
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/ident
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/valueType :db.type/keyword}
   {:db/ident :db/cardinality
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/cardinality
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/doc
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/string}
   {:db/ident :db/doc
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/string}
   {:db/ident :db/isComponent
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}
   ;; Compatibility purposes only
   {:db/ident :db/index
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}
   {:db/ident :db/fulltext
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}
   {:db/ident :db/noHistory
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}])

;;; SimpleStore Specific Stuff

(defn- clean
  "Given a map, an entity and attribute name, remove the entire attr if it is
   nil/empty, and the entire entity if it has no attrs."
  [m e a]
  (if (empty? (get-in m [e a]))
    (let [m (c/update m e dissoc a)]
      (if (empty? (get m e))
        (dissoc m e)
        m))
    m))

(defn card-one?
  "Efficiently check if a given attr is card-one?"
  [store attr]
  (contains? (:card-one-attrs (:attrs store)) attr))

(defn identity?
  "Efficiently check if a given attr is an identity attr"
  [store attr]
  (contains? (:identity-attrs (:attrs store)) attr))

(defn ref?
  "Efficiently check if a given attr is a ref"
  [store attr]
  (contains? (:ref-attrs (:attrs store)) attr))

(defn component?
  "Efficiently check if a given attr is a component"
  [store attr]
  (contains? (:component-attrs (:attrs store)) attr))

(defn transient?
  "Efficiently check if a given attr is transient"
  [store attr]
  (contains? (:transient-attrs (:attrs store)) attr))

(defn has?
  "Check if the store contains a datom"
  [store e a v]
  (when-let [val (get-in store [:index e a])]
    (or (= val v) (and (set? val) (contains? val v)))))

(defn- previous-retraction
  "Given an attribute and value, return an operation resulting in the removal
  of the 'old' value for the operation. If there is nothing to replace, return
  nil."
  [store e a]
  (when (card-one? store a)
    (let [v (get-in store [:index e a] ::not-found)]
      (when (not= v ::not-found)
        [:db/retract e a v]))))

(defn- index-insertions
  "Update the store's index with the given concrete datoms"
  [store datoms]
  (assoc store :index
               (reduce (fn [m {:keys [e a v]}]
                         (cond
                           (transient? store a) m
                           (card-one? store a) (assoc-in m [e a] v)
                           :else (update-in m [e a] (fnil conj #{}) v)))
                 (:index store)
                 datoms)))

(defn- index-retractions
  "Update the index by retracting the given concrete datoms"
  [store datoms]
  (assoc store :index
               (reduce (fn [m {:keys [e a v]}]
                         (if (card-one? store a)
                           (-> m
                             (update-in [e] dissoc a)
                             (clean e a))
                           (-> m
                             (update-in [e a] disj v)
                             (clean e a))))
                 (:index store)
                 datoms)))

(defn- op-identity
  "If a :db/add operation expresses an identity, return the tuple [[a v] e], otherwise nil"
  [store [_ e a v]]
  (when (identity? store a)
    [[a v] e]))

(defn- datom-identity
  "If a datom expresses an identity, return the tuple [[a v] e], otherwise nil"
  [store {:keys [e a v]}]
  (when (identity? store a)
    [[a v] e]))

(defn- update-identities
  "Update the identities in the given data store to reflect inserted/retracted
   datoms"
  [store insert-datoms retract-datoms]
  (let [to-insert (into {} (keep #(datom-identity store %) insert-datoms))
        to-retract (into {} (keep #(datom-identity store %) retract-datoms))]
    (assoc store :identities
                 (let [identities (:identities store)
                       identities (apply dissoc identities (keys to-retract))
                       identities (merge identities to-insert)]
                   identities))))

(let [eid (atom 10000)]
  (defn- new-eid []
    (swap! eid inc)))

(defn- collect-tempids
  "Given a seq of operations, return a set of all tempids present in the operations.

   Tempids are only detected on :db/add operations, in the 'e' and 'v' positions."
  [store ops]
  (reduce (fn [tempids [_ e a v]]
            (let [tempids (if-not (pos-int? e)
                            (conj tempids e)
                            tempids)
                  tempids (if (and (ref? store a) (not (pos-int? v)))
                            (conj tempids v)
                            tempids)]
              tempids))
    #{}
    (filter #(= :db/add (first %)) ops)))

(defn- collect-ids
  "Given a seq of operations, return a map of {e id} for all :db/add ops with
   a tempid in entity position which express an identity"
  [store ops]
  (->> ops
    (filter #(= :db/add (first %)))
    (keep #(op-identity store %))
    (map (fn [[id e]] [e id]))
    (into {})))

(defn resolve-ids
  "Given a seqence of [attr value] ids, return a map of {id eid}, either
   finding an existing eid in the store OR creating a new one."
  [store ids]
  (reduce (fn [id->eid id]
            (if (id->eid id)
              id->eid
              (let [eid (get (:identities store) id (new-eid))]
                (assoc id->eid id eid))))
    {}
    ids))

;; New, clearer tempid assignment algorithm
;; 1. Collect all the tempids in the tx to a set.
;; 2. Collect all the identities declared in the tx into a map of {tempid id}.
;; 3. For each distinct identity, find (or create) a corresponding concrete eid
;; 4. For each tempid, find (or create) a corresponding concrete eid
;; 5. Map the datoms, substituting tempids for real IDs.

(defn- resolve-tempids
  "Given a set of operations (which may contain temporary IDs), return a tuple
   of concrete operations (with tempids swapped for concrete EIDs) and the Tempid
   bindings.

   Note: only replaces tempids in :db/add operations."
  [store ops]
  (let [tids (collect-tempids store ops)
        tid->id (collect-ids store ops)
        id->eid (resolve-ids store (vals tid->id))
        bindings (into {} (map (fn [tid]
                                 [tid
                                  (or (-> tid tid->id id->eid) (new-eid))])
                            tids))
        new-ops (map (fn [op]
                       (if (not= :db/add (first op))
                         op
                         (let [[_ e a v] op
                               e (if (pos-int? e) e (bindings e))
                               v (if (and (ref? store a) (not (pos-int? v)))
                                   (bindings v)
                                   v)]
                           [:db/add e a v])))
                  ops)]
    [new-ops bindings]))

(defn- retract-entity-ops
  "Return :db/remove operations that constitute a :db.fn/retractEntity operation"
  [store eid]
  (mapcat (fn [[attr val-or-vals]]
            (if (coll? val-or-vals)
              (map (fn [v] [:db/retract eid attr v]) val-or-vals)
              [[:db/retract eid attr val-or-vals]]))
    (get-in store [:index eid])))

(defn- retract-attr-ops
  "Expand a :db.fn/retractAttr operation to its individual constituent retractions"
  [store [_ e a]]
  (mapcat (fn [val-or-vals]
            (if (coll? val-or-vals)
              (map (fn [v]
                     [:db/retract e a v])
                val-or-vals)
              [[:db/retract e a val-or-vals]]))
    (get-in store [:index e a])))

(defn- add-operation
  "Expand a :db/add operation to include all implied operations. Includes
   logic for:

   - retracting previous values of cardinality-one attrs
   - ensuring no duplicate values"
  [store [_ e a v :as op]]
  (if (has? store e a v)
    nil
    (if-let [r (previous-retraction store e a)]
      [r op]
      [op])))

(defn- retract-operation
  "Expand a :db/retract operation to include all additional retractions
   implied by :db/isComponent."
  [store [_ e a v :as op]]
  (if (component? store a)
    [op [:db.fn/retractEntity v]]
    [op]))

(defn expand-operation
  "Expand an operation into basic add/retract operations. Includes logic for
   replacing/retracting card-one attributes and eliminating duplicate facts.

   Note that tempids should already have been replaced: operations may not
   expand to operations that contain tempids."
  [store op]
  (case (first op)
    :db/add (add-operation store op)
    :db/retract (retract-operation store op)
    :db.fn/retractEntity (retract-entity-ops store (second op))
    :db.fn/retractAttr (retract-attr-ops store op)
    (throw (ex-info (str "Unknown txdata operation " (first op))
             {:op op}))))

(defn- datom
  "Convert a :db/add or :db/retract operation to a datom record"
  [[_ e a v]]
  (f/->Datom e a v))

(defrecord SimpleStore [schema index identities attrs]
  Store
  (resolve [store txdata]
    (let [ops (txdata/operations txdata)
          [concrete-ops bindings] (resolve-tempids store ops)
          basic-ops (loop [ops (set concrete-ops)]
                      (let [ops' (set (mapcat #(expand-operation store %) ops))]
                        (if (= ops ops')
                          ops
                          (recur ops'))))
          {insertions true, retractions false} (group-by #(= :db/add (first %))
                                                 basic-ops)
          insert-datoms (map datom insertions)
          retract-datoms (map datom retractions)]
      [insert-datoms retract-datoms bindings]))
  (update [store insert-datoms retract-datoms]
    (let [store (index-retractions store retract-datoms)
          store (index-insertions store insert-datoms)
          store (update-identities store insert-datoms retract-datoms)]
      store))
  (schema [store] schema))

(defn- build-schema
  "Given txdata in entity map format, return a schema map"
  [txdata]
  (->> txdata
    (filter :db/valueType)
    (map (fn [txmap]
           [(:db/ident txmap)
            (select-keys txmap [:db/cardinality
                                :db/unique
                                :db/valueType
                                :db/isComponent
                                :factui/isTransient])]))
    (into {})))

(defn- attrs-with
  "Given a schema map, return a set of attr names that have the specified key
   and value"
  [schema key val]
  (set (keep
         (fn [[attr attr-m]]
           (when (= val (get attr-m key))
             attr))
         schema)))

(defn store
  "Return an empty SimpleStore instance"
  [schema-txdata]
  (let [schema (build-schema (concat base-schema schema-txdata))]
    (->SimpleStore schema {} {}
      {:card-one-attrs (attrs-with schema :db/cardinality :db.cardinality/one)
       :identity-attrs (attrs-with schema :db/unique :db.unique/identity)
       :ref-attrs (attrs-with schema :db/valueType :db.type/ref)
       :component-attrs (attrs-with schema :db/isComponent true)
       :transient-attrs (attrs-with schema :factui/isTransient true)})))

(defn datoms
  "Given a store, return a lazy seq of all the datoms in it."
  [{index :index}]
  (mapcat (fn [[e attrs]]
            (mapcat (fn [[a v-or-vs]]
                      (if (coll? v-or-vs)
                        (map (fn [v] [e a v]) v-or-vs)
                        [[e a v-or-vs]]))
              attrs))
    index))