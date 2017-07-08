(ns factui.store
  "A datom store. For storing datoms."
  (:require [factui.facts :as f]
            #?(:cljs [cljs.core :as c]
               :clj [clojure.core :as c]))
  (:refer-clojure :exclude [update resolve]))

(defprotocol Store
  (update [this insert-datoms retract-datoms]
    "Update the store by inserting and retracting the specified concrete datoms (such as those returned by `resolve`).")
  (resolve [this datoms]
    "Given a seq of datoms (which may contain temporary IDs), resolve against
    the existing contents of the store to return a 3-tuple of:

    1. concrete tuples to add
    2. concrete tuples to remove
    3. map of tempid to concrete EIDs

    This handles all Datomic-style semantics such as upsert, tempid resolution, and preventing duplicates"))

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

(defn- resolve-tempids
  "Given a set of datoms (which may contain temporary IDs), return a tuple of
   concrete datoms (with tempids swapped for concrete entity IDs)

   Returns a tuple of a set of concrete datoms, and the tempid bindings"
  [store datoms]
  (let [tx-ids (keep #(datom-identity store %) datoms)
        tid->id (into {} (map (fn [[id tid]] [tid id]) tx-ids))
        id->eid (reduce (fn [m id]
                          (if (get m id)
                            m
                            (let [eid (get (:identities store) id (new-eid))]
                              (assoc m id eid))))
                          {}
                  (vals tid->id))]
    (reduce (fn [[datoms bindings] {:keys [e a v] :as datom}]
              (if (pos-int? e)
                [(conj datoms (f/->Datom e a v)) bindings]
                (if-let [eid (get bindings e)]
                  [(conj datoms (f/->Datom eid a v)) bindings]
                  (let [id (get tid->id e)
                        eid (if id (id->eid id) (new-eid))]
                    [(conj datoms (f/->Datom eid a v)) (assoc bindings e eid)]))))
      [#{} {}]
      datoms)))


(defrecord SimpleStore [schema index identities card-one-attrs identity-attrs]
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
      store)))

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