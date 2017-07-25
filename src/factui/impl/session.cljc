(ns factui.impl.session
  "A Clara session wrapper which builds in Datalog semantics."
  (:require [clara.rules :as cr]
            [clara.rules.engine :as eng]
            [clara.rules.listener :as l]
            [factui.impl.txdata :as txdata]
            #?(:clj [factui.facts :as f]
               :cljs [factui.facts :as f :refer [Datom]])
            #?(:clj [clojure.core.async :as a]
               :cljs [cljs.core.async :as a])
            [factui.impl.store :as store])
  #?(:clj (:import [factui.facts Datom])))

(def ^{:dynamic true
       :doc "Dynamic var used to convey an atom containing the store to the
engine internals. Depends on the engine internals running in the same thread as
the top-level API (true for the default implementation.)"} *store*)

(def ^{:dynamic true
       :doc "Dynamic var used to convey an atom containing tempid bindings to
the engine internals. Depends on the engine internals running in the same
thread as the top-level API (true for the default implementation.)"} *bindings*)

(def ^{:dynamic true
       :doc "Dynamic var used to supply a channel to the RHS of rules. A
DatomSession instance will be placed upon the channel whenever a transaction is
complete (that is, after rules have finished firing.)"} *tx-complete*)

(def ^{:dynamic true
       :doc "Dynamic var used to pass the RHS of a rule with the curren
session's ID"} *session-id*)

(defn datom? [fact] (instance? Datom fact))

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
    (swap! *store* store/update (filter datom? facts) [])
    listener)
  (alpha-activate! [listener node facts]
    listener)
  (insert-facts-logical! [listener node token facts]
    (swap! *store* store/update (filter datom? facts) [])
    listener)
  (retract-facts! [listener facts]
    (swap! *store* store/update [] (filter datom? facts))
    listener)
  (alpha-retract! [listener node facts]
    listener)
  (retract-facts-logical! [listener node token facts]
    (swap! *store* store/update [] (filter datom? facts))
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

(defrecord DatomSession [delegate store session-id]
  eng/ISession
  (insert [session facts]
    (let [{datoms true, other-facts false} (group-by datom? facts)
          resolved (store/resolve store datoms)
          [insert-datoms retract-datoms bindings] resolved]
      (when #?(:clj (bound? #'*bindings*)
               :cljs *bindings*) (swap! *bindings* merge bindings))
      (binding [*store* (atom store)]
        (DatomSession. (-> delegate
                         (eng/retract retract-datoms)
                         (eng/insert insert-datoms)
                         (eng/insert other-facts))
          @*store*
          session-id))))

  (retract [session facts]
    (binding [*store* (atom store)]
      (DatomSession. (eng/retract delegate facts) @*store* session-id)))

  (fire-rules [session]
    (binding [*store* (atom store)
              *tx-complete* (a/chan)
              *session-id* session-id]
      (let [sess (DatomSession. (eng/fire-rules delegate) @*store* session-id)]
        (a/put! *tx-complete* sess)
        (a/close! *tx-complete*)
        sess)))

  (fire-rules [session opts]
    (binding [*store* (atom store)
              *tx-complete* (a/chan)
              *session-id* session-id]
      (let [sess (DatomSession. (eng/fire-rules delegate opts) @*store* session-id)]
        (a/put! *tx-complete* sess)
        (a/close! *tx-complete*)
        sess)))

  (query [session query params]
    (eng/query delegate query params))

  (components [session]
    (eng/components delegate)))

(defn- datom
  "Given a :db/add or :db/retraction operation, create a Datom record"
  [[_ e a v]]
  (f/->Datom e a v))

(defn session
  "Create a new Datom Session from the given underlying session and store,
   with the specified Session ID."
  [base store id]
  (let [components (update (eng/components base) :listeners conj (DatomListener.))]
    (DatomSession. (eng/assemble components) store id)))

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
  "Given Datomic-style txdata, transact it to the session.

   Returns a tuple of the new session and a map of tempids to resulting entity
   IDs."
  [session txdata]
  (binding [*bindings* (atom {})]
    (let [[insertions retractions] (prep-txdata txdata)
          new-session (-> session
                        (eng/retract retractions)
                        (eng/insert insertions)
                        (eng/fire-rules))]
      [new-session @*bindings*])))

(defn transact!
  "Function to add data, from the body of a rule."
  [txdata logical?]
  (let [store @*store*
        [insertions retractions] (prep-txdata txdata)
        [insert-datoms retract-datoms _] (store/resolve store insertions)]
    (eng/retract-facts! retract-datoms)
    (eng/insert-facts! insert-datoms (not logical?))))

(defn with-id
  "Return a session based on the given session with the specified session ID"
  [session id]
  (assoc session :session-id id))