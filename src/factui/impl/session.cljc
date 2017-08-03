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
            [factui.impl.store :as store]
            #?(:clj [clojure.pprint :as pprint :refer [pprint]]))
  #?(:clj (:import [factui.facts Datom])))

(def ^{:dynamic true
       :doc "Dynamic var used to convey an atom containing the store to the
engine internals. Depends on the engine internals running in the same thread as
the top-level API (true for the default implementation.)"} *store*)

(defn datom? [fact] (instance? Datom fact))

(def ^:dynamic *registry*)
(def ^:dynamic *triggers*)

(defn- trigger
  "Trigger registered queries which match the given activation."
  [node tokens]
  (when-let [query-name (get-in node [:query :name])]
    (when-let [registered-params (get *registry* query-name)]
      (let [params (get-in node [:query :params])
            param-bindings (keep (fn [binding]
                                   (let [param-binding (select-keys binding params)]
                                     (when (contains? registered-params param-binding)
                                       param-binding)))
                             (map :bindings tokens))]
        (when-not (empty? param-bindings)
          (swap! *triggers* into (map #(vector query-name %)
                                   param-bindings)))))))

(deftype DatomListener []
  l/IPersistentEventListener
  (to-transient [listener] listener)
  l/ITransientEventListener
  (left-activate! [listener node tokens]
    (trigger node tokens)
    listener)
  (left-retract! [listener node tokens]
    (trigger node tokens)
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
  (alpha-retract! [listener node facts]
    listener)
  (insert-facts-logical! [listener node token facts]
    (swap! *store* store/update (filter datom? facts) [])
    listener)
  (retract-facts! [listener facts]
    (swap! *store* store/update [] (filter datom? facts))
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

(declare fire-rules*)
(defrecord DatomSession [delegate store registry]
  eng/ISession
  (insert [session facts]
    (binding [*store* (atom store)]
      (DatomSession. (eng/insert delegate facts) @*store* registry)))

  (retract [session facts]
    (binding [*store* (atom store)]
      (DatomSession. (eng/retract delegate facts) @*store* registry)))

  (fire-rules [session]
    (fire-rules* store registry #(eng/fire-rules delegate)))

  (fire-rules [session opts]
    (fire-rules* store registry #(eng/fire-rules delegate opts)))

  (query [session query params]
    (eng/query delegate query params))

  (components [session]
    (eng/components delegate)))

(defn- fire-rules*
  [store registry delegate-fire-rules]
  (binding [*store* (atom store)
            *registry* registry
            *triggers* (atom #{})]
    (let [sess (DatomSession. (delegate-fire-rules) @*store* registry)]
      (doseq [[query params :as t] @*triggers*]
        (let [results (eng/query sess query params)]
          (doseq [ch (get-in registry t)]
            (a/put! ch results))))
      sess)))

(defn session
  "Create a new Datom Session from the given underlying session and store,
   with the specified Session ID."
  [base store]
  (let [components (update (eng/components base) :listeners conj (DatomListener.))]
    (DatomSession. (eng/assemble components) store {})))

(defn transact
  "Given Datomic-style txdata, transact it to the session.

   Returns a tuple of the new session and a map of tempids to resulting entity
   IDs."
  [session txdata]
  (let [[insertions retractions bindings] (store/resolve (.-store session) txdata)
        new-session (-> session
                      (eng/retract retractions)
                      (eng/insert insertions)
                      (eng/fire-rules))]
    [new-session bindings]))

(defn transact!
  "Function to add data, from the body of a rule."
  [txdata logical?]
  (let [store @*store*
        [insert-datoms retract-datoms _] (store/resolve store txdata)]
    (eng/retract-facts! retract-datoms)
    (eng/insert-facts! insert-datoms (not logical?))))

(defn register
  "Register channel to recieve notification when the results of the specified query+params changes"
  [session query-name params ch]
  (update-in session [:registry query-name params] (fnil conj #{}) ch))

(defn deregister
  "Remove the registration of a channel to recieve notification when the results of the specified query+params changes"
  [session query-name params ch]
  (-> session
    (update-in [:registry query-name params] disj ch)
    (update-in [:registry query-name] #(if (empty? (get % params))
                                         (dissoc params)
                                         %))
    (update-in [:registry] #(if (empty? (get % query-name))
                              (dissoc query-name)
                              %))))