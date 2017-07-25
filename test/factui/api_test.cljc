(ns factui.api-test
  (:require
   #?(:cljs [factui.api :as api :include-macros true]
      :clj [factui.api :as api])
   #?(:clj
           [clara.rules :as cr]
      :cljs [clara.rules :as cr :include-macros true])
           [clojure.pprint :refer [pprint]]
   #?(:clj
           [clojure.test :as t :refer [deftest is testing run-tests]]
      :cljs [cljs.test :as t :refer-macros [deftest is testing run-tests]])

   #?(:cljs [factui.facts :as f :refer [Datom]] :clj [factui.facts :as f]))
   #?(:clj (:import [factui.facts Datom])))

(def test-schema
  [{:db/ident :person/id
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :person/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :person/likes
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many}
   {:db/ident :person/age
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :person/friends
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many}])

(api/defquery person-name
   "Find a person by their name"
   [:find ?name
    :in ?id
    :where [?id :person/name ?name]])

(api/defquery person-name-scalar
  "Find a person by their name as a scalar"
  [:find ?name .
   :in ?id
   :where [?id :person/name ?name]])

(api/defquery person-name-map
  "Find a person by their name"
  {:find [?name]
   :in [?id]
   :where [[?id :person/name ?name]]})

(api/defquery person-tuple
  "Find a single person, as a tuple"
  [:find [?id ?name]
   :in ?id
   :where [?id :person/name ?name]])

(api/defquery all-people
  "Find the names and ids of all people in the DB"
  [:find ?id ?name
   :where [?id :person/name ?name]])

(api/defquery all-names
  "Find the names all people in the DB as a collection"
  [:find [?name ...]
   :where [?id :person/name ?name]])

(api/defquery all-attrs
  "Find all attributes of an entity"
  [:find ?attr ?value
   :in ?id
   :where
   [?eid :person/id ?id]
   [?eid ?attr ?value]])

(api/defquery all-attrs-clara
   "Find all attributes of an entity, using a mixed Clara clause"
   [:find ?attr ?value
    :in ?id
    :where
    [?eid :person/id ?id]
    [Datom (= e ?eid) (= a ?attr) (= v ?value)]])

(api/defrule simple-rule
  "A basic, simple rule"
  [?p :person/likes "Milk"]
  =>
  (api/transact! [{:db/id ?p
                   :person/likes "Cookies"}]))

(api/defrule simple-rule-logical
  "A basic, simple rule"
  [?p :person/likes "Beer"]
  [?p :person/likes "Wine"]
  [?p :person/likes "Whisky"]
  =>
  (api/transact-logical! [{:db/id ?p
                           :person/likes "Alcohol"}]))


(api/defsession base ['factui.api-test] test-schema ::session)

(deftest simple-query
  (let [[s bindings] (api/transact base [{:db/id -99
                                          :person/name "Luke"}
                                         {:db/id -100
                                          :person/name "Alex"}])
        luke-id (bindings -99)
        alex-id (bindings -100)
        result (api/query s person-name luke-id)
        result-m (api/query s person-name-map luke-id)]
    (is (= result result-m #{["Luke"]}))
    (is (= 1 (count result)))
    (testing "multiple results"
      (let [r (api/query s all-people)]
        (is (= r #{[luke-id "Luke"]
                 [alex-id "Alex"]}))))
    (testing "collection results"
      (let [r (api/query s all-names)]
        (is (= r #{"Luke" "Alex"}))))
    (testing "scalar results"
      (let [r (api/query s person-name-scalar luke-id)]
        (is (= r "Luke"))))
    (testing "tuple results"
      (let [r (api/query s person-tuple luke-id)]
        (is (= r [luke-id "Luke"]))))))


(deftest all-attrs-query
  (let [[s bindings] (api/transact base [{:db/id -99
                                          :person/id 42
                                          :person/name "Luke"
                                          :person/likes ["Beer" "Cheese"]}])
        luke-id (bindings -99)
        result1 (api/query s all-attrs 42)
        result2 (api/query s all-attrs-clara 42)]
    (is (= result1 result2
          #{[:person/id 42]
            [:person/likes "Beer"]
            [:person/likes "Cheese"]
            [:person/name "Luke"]}))))

(deftest simple-rule-test
  (let [[s _] (api/transact base [{:db/id -99
                                          :person/id 42
                                          :person/name "Luke"
                                          :person/likes ["Milk"]}])
        result (api/query s all-attrs 42)]
    (is (= result
          #{[:person/id 42]
            [:person/likes "Milk"]
            [:person/likes "Cookies"]
            [:person/name "Luke"]}))))

(deftest simple-rule-logical-test
  (let [[s1 bindings] (api/transact base [{:db/id -99
                                           :person/id 42
                                           :person/name "Luke"
                                           :person/likes ["Beer" "Whisky"]}])
        result1 (api/query s1 all-attrs 42)
        [s2 _] (api/transact s1 [{:person/id 42
                                    :person/name "Luke"
                                    :person/likes ["Wine"]}])
        result2 (api/query s2 all-attrs 42)
        eid (get bindings -99)
        [s3 _] (api/transact s2 [[:db/retract eid :person/likes "Whisky"]])
        result3 (api/query s3 all-attrs 42)]
    (is (= result1 #{[:person/id 42]
                     [:person/likes "Beer"]
                     [:person/likes "Whisky"]
                     [:person/name "Luke"]}))
    (is (= result2 #{[:person/id 42]
                     [:person/likes "Beer"]
                     [:person/likes "Wine"]
                     [:person/likes "Whisky"]
                     [:person/likes "Alcohol"]
                     [:person/name "Luke"]}))
    (is (= result3 #{[:person/id 42]
                     [:person/likes "Beer"]
                     [:person/likes "Wine"]
                     [:person/name "Luke"]}))))













