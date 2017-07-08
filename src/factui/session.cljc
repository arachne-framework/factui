(ns factui.session
  "A Clara session wrapper which builds in Datalog semantics."
  (:require [clara.rules :as cr]
            [clara.rules.engine :as eng]
            [clara.rules.listener :as l]
            [factui.txdata :as txdata]
            [factui.facts :as f]
            [factui.store :as store]))

;; Bound in a transaction context
(def ^:dynamic *store*)

(deftype DatomListener []
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
    (swap! *store* store/update facts [])
    listener)
  (alpha-activate! [listener node facts]
    listener)
  (insert-facts-logical! [listener node token facts]
    (swap! *store* store/update facts [])
    listener)
  (retract-facts! [listener facts]
    (swap! *store* store/update [] facts)
    listener)
  (alpha-retract! [listener node facts]
    listener)
  (retract-facts-logical! [listener node token facts]
    (swap! *store* store/update [] facts)
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

(defrecord DatomSession [delegate store]
  eng/ISession
  (insert [session datoms]
    (DatomSession. (eng/insert delegate datoms) store))

  (retract [session datoms]
    (DatomSession. (eng/retract delegate datoms) store))

  (fire-rules [session]
    (DatomSession. (eng/fire-rules delegate) store))

  (fire-rules [session opts]
    (DatomSession. (eng/fire-rules delegate opts) store))

  (query [session query params]
    (eng/query delegate query params))

  (components [session]
    (eng/components delegate)))

(defn- datom
  "Given a :db/add or :db/retraction operation, create a Datom record"
  [[_ e a v]]
  (f/->Datom e a v))

(defn session
  "Create a new Datom Session with the specified schema txdata"
  [base schema-txdata]
  (let [components (update (eng/components base) :listeners conj (DatomListener.))]
    (DatomSession. (eng/assemble components) (store/store schema-txdata))))

(defn- prep-txdata
  "Given Datomic txdata, return a tuple of [insert-datoms retract datoms]"
  [txdata]
  (let [ops (txdata/txdata txdata)
        ;;TODO: support ops other than :db/add and :db/retract
        ops-by-type (group-by first ops)
        insertions (map datom (:db/add ops-by-type))
        retractions (map datom (:db/retract ops-by-type))]
    [insertions retractions]))

(defn transact
  "Given a sequence of Datomic-style operations, transact them as Datom facts
   to a DatomSession, and fire off a round of rules.

    Returns a tuple of the new session and a map of tempids to resulting
    entity IDs."
  [session txdata]
  (let [store (:store session)
        [insertions retractions] (prep-txdata txdata)
        [insert-datoms retract-datoms bindings] (store/resolve store insertions)]
    (binding [*store* (atom store)]
      (let [new-session (-> session
                          (eng/retract retractions)
                          (eng/retract retract-datoms)
                          (eng/insert insert-datoms)
                          (eng/fire-rules)
                          (assoc :store @*store*))]
        [new-session bindings]))))

(defn transact!
  "Function to add data, from the body of a rule."
  [txdata logical?]
  (let [store @*store*
        [insertions retractions] (prep-txdata txdata)
        [insert-datoms retract-datoms _] (store/resolve store insertions)]
    (eng/retract-facts! retract-datoms)
    (eng/insert-facts! insert-datoms (not logical?))))