(ns factui.impl.store
  "A datom store. For storing datoms."
  (:require #?(:cljs [factui.facts :as f :refer [Datom]]
               :clj [factui.facts :as f])
            #?(:cljs [cljs.core :as c]
               :clj [clojure.core :as c]))
  (:refer-clojure :exclude [update resolve])
  #?(:clj (:import [factui.facts Datom])))

(defprotocol Store
  (update [this insert-datoms retract-datoms]
    "Update the store by inserting and retracting the specified concrete datoms (such as those returned by `resolve`).")
  (resolve [this datoms]
    "Given a seq of datoms (which may contain temporary IDs), resolve against
    the existing contents of the store to return a 3-tuple of:

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

(defn- card-one?
  "Efficiently check if a given attr is card-one?"
  [store attr]
  (contains? (:card-one-attrs store) attr))

(defn- identity?
  "Efficiently check if a given attr is an identity attr"
  [store attr]
  (contains? (:identity-attrs store) attr))

(defn- ref?
  "Efficiently check if a given attr is a ref"
  [store attr]
  (contains? (:ref-attrs store) attr))

(defn- has?
  "Check if the store contains a datom"
  [store {:keys [e a v]}]
  (when-let [val (get-in store [:index e a])]
    (or (= val v) (and (set? val) (contains? val v)))))

(defn- replaces
  "Return any datom that would be replaced by the given datom in the store.
   Returns nil if the given datom would not replace any existing ones."
  [store {:keys [e a]}]
  (when (card-one? store a)
    (let [v (get-in store [:index e a] ::not-found)]
      (when (not= v ::not-found)
        (f/->Datom e a v)))))

(defn- index-insertions
  "Update the store's index with the given concrete datoms"
  [store datoms]
  (assoc store :index
               (reduce (fn [m {:keys [e a v]}]
                         (if (card-one? store a)
                           (assoc-in m [e a] v)
                           (update-in m [e a] (fnil conj #{}) v)))
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
  "Given a seq of datoms, return a set of all tempids present in the datoms (e
   or v position)"
  [store datoms]
  (reduce (fn [tempids {:keys [e a v]}]
            (let [tempids (if-not (pos-int? e)
                            (conj tempids e)
                            tempids)
                  tempids (if (and (ref? store a) (not (pos-int? v)))
                            (conj tempids v)
                            tempids)]
              tempids))
    #{}
    datoms))

(defn- collect-ids
  "Given a seq of datoms, return a map of {e id} for all datoms which express
   an identity"
  [store datoms]
  (->> datoms
    (keep #(datom-identity store %))
    (map (fn [[id e]] [e id]))
    (into {})))

(defn resolve-ids
  "Given a seqence of [attr value] ids, return a map of {id eid}, either
   finding an existing eid in the store or creating a new one."
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
  "Given a set of datoms (which may contain temporary IDs), return a tuple of
   concrete datoms (with tempids swapped for concrete entity IDs)

   Returns a tuple of a set of concrete datoms, and the tempid bindings"
  [store datoms]
  (let [tids (collect-tempids store datoms)
        tid->id (collect-ids store datoms)
        id->eid (resolve-ids store (vals tid->id))
        bindings (into {} (map (fn [tid]
                                 [tid
                                  (or (-> tid tid->id id->eid) (new-eid))])
                            tids))
        new-datoms (map (fn [{:keys [e a v]}]
                          (let [e (if (pos-int? e) e (bindings e))
                                v (if (and (ref? store a) (not (pos-int? v)))
                                    (bindings v)
                                    v)]
                            (f/->Datom e a v)))
                     datoms)]
    [new-datoms bindings]))

(defrecord SimpleStore [schema index identities card-one-attrs identity-attrs ref-attrs]
  Store
  (resolve [store datoms]
    (let [[datoms bindings] (resolve-tempids store datoms)
          datoms (filter #(not (has? store %)) datoms)
          retractions (keep #(replaces store %) datoms)]
      [datoms retractions bindings]))

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
            (select-keys txmap [:db/cardinality :db/unique :db/valueType :factui/transient])]))
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
      (attrs-with schema :db/cardinality :db.cardinality/one)
      (attrs-with schema :db/unique :db.unique/identity)
      (attrs-with schema :db/valueType :db.type/ref))))

;; TODO: Plan
;; Pass around the store, updating it and using it to filter whenever anythign is changed.
;;   explicit/logical insertion
;;   explicit/logical retraction

(defn fact-type-fn
  "Given a store, return a function that will return the type of a fact"
  [store]
  (fn [fact]
    (let [t (type fact)]
      (if (= Datom t)
        (.-a fact)
        t))))

(defn ancestors-fn
  "Given a store, return a function that will return the ancestors of a type"
  [store]
  ancestors
  (let [h (make-hierarchy)]
    (fn [type]
      (if (keyword? type)
        [Datom]
        (ancestors type)))))

