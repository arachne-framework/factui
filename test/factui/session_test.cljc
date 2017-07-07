(ns factui.session-test
  (:require
    #?(:cljs [factui.api :as api :include-macros true]
       :clj [factui.api :as api])
   #?(:clj [clara.rules :as cr]
       :cljs [clara.rules :as cr :include-macros true])
    [clojure.pprint :refer [pprint]]
   #?(:clj [clojure.test :as t :refer [deftest is testing run-tests]]
      :cljs [cljs.test :as t :refer-macros [deftest is testing run-tests]])
   #?(:cljs [factui.facts :as f :refer [Datom]]
      :clj [factui.facts :as f]))
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
    :db/cardinality :db.cardinality/one}])

(cr/defquery person-by-pid
  [:?pid]
  [Datom (= e ?p) (= a :person/id) (= v ?pid)]
  [Datom (= e ?p) (= a ?a) (= v ?v)])

(cr/defrule popeye-likes-spinach
  [Datom (= e ?e) (= a :person/name) (= v "Popeye")]
  =>
  (let [d (f/->Datom ?e :person/likes "spinach")]
    (api/transact! [d])))

#?(:cljs (enable-console-print!))

(api/defsession base ['factui.session-test] test-schema)

(deftest basic-insertion
  (let [s (api/transact-all base [{:person/id 42
                                   :person/name "Luke"}])
        results (cr/query s person-by-pid :?pid 42)]
    (is (= (set (map #(select-keys % [:?a :?v]) results))
          #{{:?a :person/name :?v "Luke"}
            {:?a :person/id :?v 42}}))))

(deftest tempids-resolve-to-same-entity
  (let [s (api/transact-all base [{:db/id -47
                                   :person/name "Luke"}
                                  {:db/id -47
                                   :person/id 42
                                   :person/age 32}])

        results (cr/query s person-by-pid :?pid 42)]
    (is (= (set (map #(select-keys % [:?a :?v]) results))
          #{{:?a :person/name :?v "Luke"}
            {:?a :person/age :?v 32}
            {:?a :person/id :?v 42}}))))

(deftest identity-attribute-resolution
  (let [s1 (api/transact-all base [{:person/id 42
                                    :person/name "Luke"}
                                   {:person/id 42
                                    :person/age 32}])
        s2 (api/transact-all base [{:person/id 42
                                    :person/name "Luke"}]
                                  [{:person/id 42
                                    :person/age 32}])
        results1 (cr/query s1 person-by-pid :?pid 42)
        results2 (cr/query s2 person-by-pid :?pid 42)]
    (is (= (set (map #(select-keys % [:?a :?v]) results1))
           (set (map #(select-keys % [:?a :?v]) results2))
           #{{:?a :person/name :?v "Luke"}
             {:?a :person/age :?v 32}
             {:?a :person/id :?v 42}}))))

(deftest no-duplicate-facts
  (let [s (api/transact-all base [{:person/id 42
                                   :person/name "Luke"}]
                                 [{:person/id 42
                                   :person/name "Luke"}])
        results (cr/query s person-by-pid :?pid 42)]
    (is (= 2 (count results)))))

(deftest card-many
  (let [s (api/transact-all base [{:person/id 42
                                   :person/likes #{"beer" "meat"}}]
                                  [{:person/id 42
                                    :person/likes #{"cheese"}}])
        results (cr/query s person-by-pid :?pid 42)]
    (is (= (set (map #(select-keys % [:?a :?v]) results))
           #{{:?a :person/id :?v 42}
             {:?a :person/likes :?v "beer"}
             {:?a :person/likes :?v "cheese"}
             {:?a :person/likes :?v "meat"}}))))

(deftest card-one
  (let [s (api/transact-all base [{:person/id 42
                                   :person/name "Luke"}]
                                 [{:person/id 42
                                  :person/name "Lukas"}])
        results (cr/query s person-by-pid :?pid 42)]
    (is (= (set (map #(select-keys % [:?a :?v]) results))
           #{{:?a :person/name :?v "Lukas"}
             {:?a :person/id :?v 42}}))))

(deftest basic-logic
  (let [s (api/transact-all base [{:person/id 42
                                   :person/name "Popeye"}])
        results (cr/query s person-by-pid :?pid 42)]
    (is (= (set (map #(select-keys % [:?a :?v]) results))
          #{{:?a :person/name :?v "Popeye"}
            {:?a :person/id :?v 42}
            {:?a :person/likes :?v "spinach"}}))))

(deftest duplicate-entailed-facts
  (testing "implied followed by explicit"
    (let [s (api/transact-all base [{:person/id 42
                                     :person/name "Popeye"}]
                                   [{:person/id 42
                                     :person/likes "spinach"}])
          results (cr/query s person-by-pid :?pid 42)]
      (is (= 3 (count results)))))
  (testing "explicit followed by implied"
    (let [s (api/transact-all base [{:person/id 42
                                     :person/lies "spinach"}]
                                   [{:person/id 42
                                     :person/name "Popeye"}])
          results (cr/query s person-by-pid :?pid 42)]
      (is (= 3 (count results))))))