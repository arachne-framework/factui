(ns factui.rules
  (:require
    [factui.facts :as f]
    #?(:clj [clara.rules :as cr]
       :cljs [clara.rules :as cr :include-macros true])
    [clara.rules.accumulators :as acc]
    [clojure.spec.alpha :as s]
    [clojure.walk :as w]
    [clojure.string :as str]
    [factui.specs.datalog :as ds]))

(let [eid (atom 1)]
  (defn new-eid
    "Return a new, unique EID"
    []
    (swap! eid inc)))

(def ^:dynamic *tempid-bindings*)
(def ^:dynamic *bootstrap* false)

(cr/defrule clean-up-transactions-rule
  "As the final rule, clean up by retracting all remaining transactional
   facts, as they are not intended to be a permanent part of the fact
   store."
  {:salience -100}
  [?ops <- (acc/all) :from [::f/operation [{:keys [op args]}]]]
  [?bindings <- (acc/all) :from [::f/tempid-binding]]
  =>
  (when-not (empty? ?ops) (apply cr/retract! ?ops))
  (when-not (empty? ?bindings) (apply cr/retract! ?bindings)))

(cr/defrule enforce-schema-rule
  "Thrown an exception when trying to add an attribute not in the schema"
  [?op <- ::f/operation [{:keys [op args]}] (= op ?op) (= ?a (second args))]
  [:test (contains? #{:db/add :db/retract} ?op)]
  [:not [::f/attribute [{:keys [ident identity? card-one?]}] (= ident ?a)]]
  =>
  (when-not factui.rules/*bootstrap*
    (throw (ex-info (str "Unknown attribute " ?a) {:attr ?a
                                                   :operation ?op}))))

(cr/defrule missing-entity-rule
  "Insertions to a concrete entity ID not present in the DB should blow up"
  [::f/operation [{:keys [op args]}] (= op :db/add) (= ?e (first args))]
  [:test (pos-int? ?e)]
  [:not [::f/datom [{:keys [e a v]}] (= e ?e)]]
  =>
  (throw (ex-info "Entity does not exist" {:eid ?e})))

(cr/defrule insertions-rule
  "Rule to transform :db/add operations to actual datoms"
  {:salience -10}
  [?op <- ::f/operation [{op :op [e a v] :args}] (= op :db/add) (= ?e e) (= ?a a) (= ?v v)]
  [:or [:and [:test (not (pos-int? ?e))]
             [::f/tempid-binding [{:keys [tempid eid]}] (= tempid ?e) (= ?eid eid)]]
       [:and [:test (pos-int? ?e)]
             [::f/datom [{:keys [e a v]}] (= e ?e) (= e ?eid)]]]
  [:not [::f/datom [{:keys [e a v]}] (= e ?eid) (= a ?a) (= v ?v)]]
  =>
  (cr/retract! ?op)
  (cr/insert-unconditional! (f/->Datom ?eid ?a ?v)))

(cr/defrule insertions-with-old-value
  "When inserting a card-one attribute that has an old value, retract the old value."
  {:salience -9}
  [?op <- ::f/operation [{op :op [e a v] :args}] (= op :db/add) (= ?e e) (= ?a a) (= ?v v)]
  [::f/attribute [{:keys [ident card-one?]}] (= ident ?a) (= true card-one?)]
  [:or [:and [:test (not (pos-int? ?e))]
             [::f/tempid-binding [{:keys [tempid eid]}] (= tempid ?e) (= ?eid eid)]]
       [:and [:test (pos-int? ?e)]
             [::f/datom [{:keys [e a v]}] (= e ?e) (= e ?eid)]]]
  [?old <- ::f/datom [{:keys [e a v]}] (= e ?eid) (= a ?a) (= v ?old-v)]
  [:test (not= ?old-v ?v)]
  =>
  (cr/retract! ?op)
  (cr/retract! ?old)
  (cr/insert-unconditional! (f/->Datom ?eid ?a ?v)))

(cr/defrule unify-identity-tempids-rule
  "If two or more operations in the same transaction have the same identity
   attr, they are the same and should have the same tempid"
  {:salience 50}
  [::f/attribute  [{:keys [ident identity? card-one?]}](= ident ?ident-attr) (= true identity?)]
  [::f/operation [{:keys [op args]}] (= op :db/add)
                          (= (first args) ?tempid1)
                          (= (second args) ?ident-attr)
                          (= (nth args 2) ?ident-val)]
  [::f/operation [{:keys [op args]}] (= op :db/add)
                          (= (first args) ?tempid2)
                          (= (second args) ?ident-attr)
                          (= (nth args 2) ?ident-val)]
  [:test (not= ?tempid1 ?tempid2)]
  [:not [::f/tempid-binding [{:keys [tempid]}] (= tempid ?tempid2)]]
  [?all-e <- (acc/all) :from [::f/operation [{:keys [op args]}] (= op :db/add) (= (first args) ?tempid2)]]
  [?all-v <- (acc/all) :from [::f/operation [{:keys [op args]}] (= op :db/add) (= (nth args 2) ?tempid2)]]
  =>
  (when-not (and (empty? ?all-e) (empty? ?all-v))
    (let [new-e-facts (for [fact ?all-e]
                        (let [[_ a v] (:args fact)]
                          (f/->Operation :db/add [?tempid1 a v])))
          new-v-facts (for [fact ?all-v]
                        (let [[e a _] (:args fact)]
                          (f/->Operation :db/add [e a ?tempid1])))]
      (apply cr/retract! (concat ?all-e ?all-v))
      (cr/insert-all-unconditional! (concat new-e-facts new-v-facts)))))

(cr/defrule assign-free-tempid-rule
  "Create a tempid binding for an unconstrainted tempid"
  [?ops <- (acc/all) :from [::f/operation [{:keys [op args]}] (= op :db/add) (= ?tid (first args))]]
  [:not [::f/tempid-binding [{:keys [tempid]}] (= tempid ?tid)]]
  [:test (not (pos-int? ?tid))]
  =>
  (let [eid (factui.rules/new-eid)]
    (swap! factui.rules/*tempid-bindings* assoc ?tid eid)
    (cr/insert-unconditional! (f/->TempidBinding ?tid eid))))

(cr/defrule assign-identity-tempid-rule
  "Create a tempid binding for an ident attr that already exists in the DB"
  {:salience 100}
  [::f/operation [{op :op [e a v] :args}] (= op :db/add) (= ?tid e) (= ?a a) (= ?v v)]
  [:test (not (pos-int? ?tid))]
  [:not [::f/tempid-binding [{:keys [tempid]}] (= tempid ?tid)]]
  [::f/attribute  [{:keys [ident identity? card-one?]}] (= ident ?attr) (= true identity?)]
  [::f/datom [{:keys [e a v]}]  (= e ?eid) (= a ?attr) (= v ?v)]
  =>
  (swap! factui.rules/*tempid-bindings* assoc ?tid ?eid)
  (cr/insert-unconditional! (f/->TempidBinding ?tid ?eid)))

(cr/defrule schema-insertion-rule
  "Adds attribute facts to the DB when schema txdata is transacted"
  [::f/datom [{:keys [e a v]}]  (= e ?e) (= a :db/valueType)]
  [?entity-datoms <- (acc/all) :from [::f/datom [{:keys [e a v]}]  (= e ?e)]]
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
    ;; TODO: Build rules to enforce key ::f/datom [{:keys [e a v]}] ic semantics (DONE)
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