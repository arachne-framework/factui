(ns factui.store
  "A datom store. For storing datoms."
  (:require [factui.facts :as f]))

(defprotocol Store
  (insert [this datoms]
    "Insert the datoms into the store, returning a 4-tuple of the updated store, facts to insert, facts to retract, and the tempid bindings.")
  (retract [this datoms]
    "Retract the specified datoms from the store, returning the updated store."))

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
    (let [m (update m e dissoc a)]
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
    (when-let [v (get-in store [:index e a])]
      (f/->Datom e a v))))

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

(defn- find-identity
  "Given a store and a set of datoms with the same eid, return a [attr value]
   identity for the datom set, if present."
  [store datoms]
  (first (keep (fn [datom]
                 (let [a (.-a datom)]
                   ()
                   (when (identity? store a)
                     [a (.-v datom)])))
           datoms)))

(let [eid (atom 10000)]
  (defn- new-eid []
    (swap! eid inc)))

(defn- assign-eid
  "Given a seq of datoms, set the e of each datom to the specified value"
  [datoms e]
  (map (fn [d] (assoc d :e e)) datoms))

(defn- resolve-tempids
  "Given a set of datoms which may contain temporary IDs, assign appropriate
   tempids following Datomic datalog semantics.

   Returns a 3tuple of the updated store and concrete datoms."
  [store datoms]
  (reduce (fn [[store result-datoms bindings] [tid datoms]]
            (if (pos-int? tid)
              [store bindings (into result-datoms datoms)]
              (let [identity (find-identity store datoms)
                    existing-eid (when identity
                                   (get-in store [:identities identity]))
                    eid (or existing-eid (new-eid))
                    new-store (cond
                                 (not identity) store
                                 existing-eid store
                                 :else (assoc-in store [:identities identity]
                                         eid))
                    new-bindings (assoc bindings tid eid)
                    new-datoms (into result-datoms (assign-eid datoms eid))]
                [new-store new-datoms new-bindings])))
    [store #{} {}]
    (group-by (fn [d] (.-e d)) datoms)))

(defrecord SimpleStore [schema index identities card-one-attrs identity-attrs]
  Store
  (insert [store datoms]
    (let [[store datoms bindings] (resolve-tempids store datoms)
          datoms (filter #(not (has? store %)) datoms)
          retractions (keep #(replaces store %) datoms)
          store (index-retractions store datoms)
          store (index-insertions store datoms)]
      [store datoms retractions bindings]))
  (retract [store datoms]
    (index-retractions store datoms)))

(defn- build-schema
  "Given txdata in entity map format, return a schema map"
  [txdata]
  (->> txdata
    (filter :db/valueType)
    (map (fn [txmap]
           [(:db/ident txmap)
            (select-keys txmap [:db/cardinality :db/unique])]))
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
      (attrs-with schema :db/unique :db.unique/identity))))

;; TODO: Plan
;; Pass around the store, updating it and using it to filter whenever anythign is changed.
;;   explicit/logical insertion
;;   explicit/logical retraction