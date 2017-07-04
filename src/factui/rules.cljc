(ns factui.rules
  (:require
    [factui.facts :as f]
    [clara.rules :as cr]
    [clara.rules.accumulators :as acc]
    [clojure.spec.alpha :as s]
    [clojure.walk :as w]
    [clojure.string :as str]
    [factui.specs.datalog :as ds]
    )
  #?(:cljs (:require-macros [clara.rules :as cr]))

  )

(let [eid (atom 1)]
  (defn new-eid
    "Return a new, unique EID"
    []
    (swap! eid inc)))


(def ^:dynamic *tempid-bindings*)
(def ^:dynamic *bootstrap* false)

;;;;;;;;;;;;;;;;;;; Rules to implement Datomic features

(cr/defrule clean-up-transactions-rule
  "As the final rule, clean up by retracting all remaining transactional
   facts, as they are not intended to be a permanent part of the fact
   store."
  {:salience -100}
  [?ops <- (acc/all) :from [factui.facts.Operation]]
  [?bindings <- (acc/all) :from [factui.facts.TempidBinding]]
  =>
  (when-not (empty? ?ops) (apply cr/retract! ?ops))
  (when-not (empty? ?bindings) (apply cr/retract! ?bindings)))

(cr/defrule enforce-schema-rule
  "Thrown an exception when trying to add an attribute not in the schema"
  [?op <- factui.facts.Operation (= op ?op) (= ?a (second args))]
  [:test (contains? #{:db/add :db/retract} ?op)]
  [:not [factui.facts.Attribute (= ident ?a)]]
  =>
  (when-not *bootstrap*
    (throw (ex-info (str "Unknown attribute " ?a) {:attr ?a
                                                   :operation ?op}))))

(cr/defrule missing-entity-rule
  "Insertions to a concrete entity ID not present in the DB should blow up"
  [factui.facts.Operation (= op :db/add) (= ?e (first args))]
  [:test (pos-int? ?e)]
  [:not [factui.facts.Datom (= e ?e)]]
  =>
  (throw (ex-info "Entity does not exist" {:eid ?e})))

(cr/defrule insertions-rule
  "Monster rule to handle all :db/add operations, in all their various
   flavors."
  {:salience -10}
  [:or
   ;; tempid case
   [:and
    [?op <- factui.facts.Operation [{op :op [e a v] :args}] (= op :db/add) (= ?tid e) (= ?a a) (= ?v v)]
    [:test (not (pos-int? ?tid))]
    [factui.facts.TempidBinding (= tempid ?tid) (= ?e eid)]]
   ;; concrete eid case
   [:and
    [?op <- factui.facts.Operation [{op :op [e a v] :args}] (= op :db/add) (= ?e e) (= ?a a) (= ?v v)]
    [factui.facts.Datom (= e ?e)]
    ]]
  [:not [factui.facts.Datom (= e ?e) (= a ?a) (= v ?v)]]
  [:or
   ;; Cardinality many
   [:not [factui.facts.Attribute (= ident ?a) (= true card-one?)]]
   ;; Cardinality one without old value
   [factui.facts.Attribute (= ident ?a) (= true card-one?)]
   ;; Cardinality one with old value
   [:and
    [factui.facts.Attribute (= ident ?a) (= true card-one?)]
    [?old <- factui.facts.Datom (= e ?e) (= a ?a)]]]
  =>
  (cr/retract! ?op)
  (when ?old
    (cr/retract! ?old))
  (cr/insert-unconditional! (f/->Datom ?e ?a ?v)))

(cr/defrule unify-identity-tempids-rule
  "If two or more operations in the same transaction have the same identity
   attr, they are the same and should have the same tempid"
  {:salience 50}
  [factui.facts.Attribute (= ident ?ident-attr) (= true identity?)]
  [factui.facts.Operation (= op :db/add)
                          (= (first args) ?tempid1)
                          (= (second args) ?ident-attr)
                          (= (nth args 2) ?ident-val)]
  [factui.facts.Operation (= op :db/add)
                          (= (first args) ?tempid2)
                          (= (second args) ?ident-attr)
                          (= (nth args 2) ?ident-val)]
  [:test (not= ?tempid1 ?tempid2)]
  [:not [factui.facts.TempidBinding (= tempid ?tempid2)]]
  [?all-e <- (acc/all) :from [factui.facts.Operation (= op :db/add) (= (first args) ?tempid2)]]
  [?all-v <- (acc/all) :from [factui.facts.Operation (= op :db/add) (= (nth args 2) ?tempid2)]]
  =>
  (when-not (and (empty? ?all-e) (empty? ?all-v))
    (let [new-e-facts (for [fact ?all-e]
                        (let [[_ a v] (:args fact)]
                          (factui.facts/->Operation :db/add [?tempid1 a v])))
          new-v-facts (for [fact ?all-v]
                        (let [[e a _] (:args fact)]
                          (factui.facts/->Operation :db/add [e a ?tempid1])))]
      (apply cr/retract! (concat ?all-e ?all-v))
      (cr/insert-all-unconditional! (concat new-e-facts new-v-facts)))))

(cr/defrule assign-free-tempid-rule
  "Create a tempid binding for an unconstrainted tempid"
  [?ops <- (acc/all) :from [factui.facts.Operation (= op :db/add) (= ?tid (first args))]]
  [:not [factui.facts.TempidBinding (= tempid ?tid)]]
  [:test (not (pos-int? ?tid))]
  =>
  (let [eid (new-eid)]
    (swap! *tempid-bindings* assoc ?tid eid)
    (cr/insert-unconditional! (f/->TempidBinding ?tid eid))))

(cr/defrule assign-identity-tempid-rule
  "Create a tempid binding for an ident attr that already exists in the DB"
  {:salience 100}
  [factui.facts.Operation [{op :op [e a v] :args}] (= op :db/add) (= ?tid e) (= ?a a) (= ?v v)]
  [:test (not (pos-int? ?tid))]
  [:not [factui.facts.TempidBinding (= tempid ?tid)]]
  [factui.facts.Attribute (= ident ?attr) (= true identity?)]
  [factui.facts.Datom (= e ?eid) (= a ?attr) (= v ?v)]
  =>
  (swap! *tempid-bindings* assoc ?tid ?eid)
  (cr/insert-unconditional! (f/->TempidBinding ?tid ?eid)))

(cr/defrule schema-insertion-rule
  "Adds attribute facts to the DB when schema txdata is transacted"
  [factui.facts.Datom (= e ?e) (= a :db/valueType)]
  [?entity-datoms <- (acc/all) :from [factui.facts.Datom (= e ?e)]]
  =>
  (let [entity (reduce (fn [acc datom]
                         (assoc acc (:a datom) (:v datom)))
                 {:db/id ?e}
                 ?entity-datoms)]
    (cr/insert-unconditional!
      (f/map->Attribute
        {:ident (:db/ident entity)
         :type (:db/valueType entity)
         :card-one? (not= :db.cardinality/many (:db/cardinality entity))
         :identity? (= :db.unique/identity (:db/unique entity))}))))


;; TODO: Build txdata interface (DONE)
    ;; TODO: Build rules to enforce key Datomic semantics (DONE)
    ;; - No new entity IDs (DONE)
    ;; - Cardinality 1 (DONE)
    ;; - Tempids & upsert with db/identity (DONE)
    ;; - Tx-functions (mechanism in place)
    ;; - Component Retraction (TODO)
    ;; TODO: Write some tests
    ;; TODO: POSSIBLE: Write a pull expression aggregate
    ;; TODO: POSSIBLE: Build more datomic-like API (with :find clause & aggs)
     ; TODO: POSSIBLE: Build more datomic-like aggregates API
    ;; TODO: Do some benchmarks
    ;; TODO: Build some UI/React Wrappers