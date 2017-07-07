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

;; Prevent duplicate datoms ; DONE
;; Enforce cardinality one  ; DONE
;; Enforce schema           ; TODO

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

    listener)
  (alpha-activate! [listener node facts]
    listener)
  (insert-facts-logical! [listener node token facts]

    listener)
  (retract-facts! [listener facts]

    listener)
  (alpha-retract! [listener node facts]
    listener)
  (retract-facts-logical! [listener node token facts]

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
        store (:store session)
        [store insert-facts retract-facts bindings] (store/insert store insertions)
        store (store/retract store retractions)]
    (binding [*store* (atom store)]
      (let [session (-> session
                      (eng/insert facts)
                      (eng/fire-rules))
            session (assoc session :store @*store*)]
        [session bindings]))))

;; Question: should I be *listening* to actually update the index, as opposed to updating it as soon as I know I am "going to"?

;; I think so. Store/insert, as used, should only be used for tempid resolution.

;; Updating the index is a completely separate task and should be strictly from *observing* (via the listener).

;; Thus, we can decouple the tempid-resolution (to get concrete datoms) from updating the index.

;;This works as long as we don't do multiple writes without an actual insert or firing rules. But that's always true when using "transact".


