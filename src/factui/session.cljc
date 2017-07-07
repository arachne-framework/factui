(ns factui.session
  "A Clara session wrapper which builds in Datalog semantics."
  (:require [clara.rules :as cr]
            [clara.rules.engine :as eng]
            [clara.rules.listener :as l]
            [factui.facts :as f]
            [factui.store :as store]))

;; Tempid Resolution  ; DONE
;; - free
;; - constrained by identity attr
;;   - by existing datoms
;;   - by datoms in same tx

;; Prevent duplicate datoms ; TODO
;; Enforce cardinality one  ; TODO
;; Enforce schema           ; TODO

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

(deftype DatomicListener []
  l/IPersistentEventListener
  (to-transient [listener] listener)
  l/ITransientEventListener
  (left-activate! [listener node tokens]
    listener)
  (left-retract! [listener node tokens]
    listener)
  (right-activate! [listener node elements]
    listener)
  (right-retract! [listener node elements]
    listener)
  (insert-facts! [listener facts]
    ;(println "l:insert" facts)
    listener)
  (alpha-activate! [listener node facts]
    listener)
  (insert-facts-logical! [listener node token facts]
   ; (println "l:insert logical" facts)
    listener)
  (retract-facts! [listener facts]
   ; (println "l:retract" facts)
    listener)
  (alpha-retract! [listener node facts]
    listener)
  (retract-facts-logical! [listener node token facts]
   ; (println "l:retract logical" facts)
    listener)
  (add-accum-reduced! [listener node join-bindings result fact-bindings]
    listener)
  (remove-accum-reduced! [listener node join-bindings fact-bindings]
    listener)
  (add-activations! [listener node activations]
    listener)
  (remove-activations! [listener node activations]
    listener)
  (fire-rules! [listener node]
    listener)
  (to-persistent! [listener]
    listener))

(let [eid (atom 10000)]
  (defn- new-eid []
    (swap! eid inc)))

(defn- identity?
  "Test of an attribute is an identity attribute"
  [schema attr]
  (-> schema attr :db/unique (= :db.unique/identity)))

(defn- find-identity
  "Given a set of datoms, return a [attr value] identity, if present."
  [datoms schema]
  (first (keep (fn [datom] (let [a (.-a datom)]
                             (when (identity? schema a)
                               [a (.-v datom)])))
           datoms)))

(defn- assign-eid
  "Given a seq of datoms, set the e of each datom to the specified value"
  [datoms e]
  (map (fn [d] (assoc d :e e)) datoms))

(defrecord DatomSession [delegate schema identities store]
  eng/ISession
  (insert [session datoms]
    (DatomSession. (eng/insert delegate datoms) schema identities store))

  (retract [session datoms]
    (DatomSession. (eng/retract delegate datoms) schema identities store))

  (fire-rules [session]
    (DatomSession. (eng/fire-rules delegate) schema identities store))

  (fire-rules [session opts]
    (DatomSession. (eng/fire-rules delegate opts) schema identities store))

  (query [session query params]
    (eng/query delegate query params))

  (components [session]
    (eng/components delegate)))

(defn- build-schema
  "Given txdata in entity map format, return a schema map"
  [txdata]
  (->> txdata
    (filter :db/valueType)
    (map (fn [txmap]
           [(:db/ident txmap)
            (select-keys txmap [:db/cardinality :db/unique])]))
    (into {})))

(defn- datom
  "Given a :db/add or :db/retraction operation, create a Datom record"
  [[_ e a v]]
  (f/->Datom e a v))

;; TODO: This could be substantially faster using transients, if necessary
(defn- assign-tempids
  "Given a set of datoms, assign appropriate tempids. Return a 3-tuple with the
   following values:

   1. identities - the updated identities map
   2. bindings - map of tempid -> entity ID bindings
   3. datoms - set of datoms with concrete entity IDs

   s:tuple of an updated identities map and a seq of datoms with
   concrete entity IDs."
  [datoms schema identities]
  (reduce (fn [[identities bindings result-datoms] [tid datoms]]
            (if (pos-int? tid)
              [identities bindings (into result-datoms datoms)]
              (let [identity (find-identity datoms schema)
                    existing-eid (when identity (identities identity))
                    eid (or existing-eid (new-eid))
                    next-identities (cond
                                      (not identity) identities
                                      existing-eid identities
                                      :else (assoc identities identity eid))
                    new-bindings (assoc bindings tid eid)
                    new-datoms (into result-datoms (assign-eid datoms eid))]
                [next-identities new-bindings new-datoms])))
    [identities {} #{}]
    (group-by (fn [d] (.-e d)) datoms)))

(defn- retract-datoms
  "Update a DatomicSession, updating not only RETE nodes but the datom
   set as well."
  [session datoms]
  (if (empty? datoms)
    session
    (let [session (update session :store store/retract datoms)]
      (eng/retract session datoms))))

(defn- insert-datoms
  "Update a DatomicSession, updating not only RETE nodes but the datom
   set and identity set as well"
  [session datoms]
  (if (empty? datoms)
    datoms
    (let [store (:store session)
          [identities bindings datoms] (assign-tempids datoms
                                         (:schema session)
                                         (:identities session))
          datoms (filter #(not (store/has? store %)) datoms)
          retractions (keep #(store/replaces store %) datoms)
          session (retract-datoms session retractions)
          session (if (empty? datoms)
                    session
                    (eng/insert session datoms))
          session (assoc session :identities identities)
          session (update session :store store/insert datoms)]
      [session bindings])))


(defn session
  "Create a new Datom Session with the specified schema txdata"
  [base schema-txdata]
  (let [schema (build-schema (concat base-schema schema-txdata))
        components (update (eng/components base) :listeners conj (DatomicListener.))]
    (DatomSession. (eng/assemble components) schema {} (store/store schema))))

(defn transact
  "Given a sequence of Datomic-style operations, transact them as Datom facts
   to a DatomSession, and fire off a round of rules.

    Returns a tuple of the new session and a map of tempids to resulting
    entity IDs."
  [session operations]
  (let [ops-by-type (group-by first operations)
        ;;TODO: support ops other than :db/add and :db/retraction here
        insertions (map datom (:db/add ops-by-type))
        retractions (map datom (:db/retract ops-by-type))
        [session bindings] (insert-datoms session insertions)
        session (retract-datoms session retractions)
        session (eng/fire-rules session)]
    [session bindings]))

(comment

  (def schema [{:db/ident :person/name
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/one}
               {:db/ident :person/likes
                :db/valueType :db.type/string
                :db/cardinality :db.cardinality/many}
               {:db/ident :person/id
                :db/valueType :db.type/long
                :db/cardinality :db.cardinality/one
                :db/unique :db.unique/identity}])


  (cr/defsession base 'factui.session)
  (def s (session base schema))

  (def r (transact s (txdata/txdata [{:person/id 42
                                :person/name "luke"
                                :person/likes #{"beer" "meat"}}
                               {:person/id 43
                                :person/likes #{"cheese"}}
                               ])))


  (pprint r)

  )