(ns factui.factui-test
  (:require
   [factui.rules :as r]
   #?(:clj [clojure.test :as t :refer [deftest is testing run-tests]])
   #?(:cljs [cljs.test :as t :refer-macros [deftest is testing run-tests]])))

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
   {:db/ident :person/likes
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many}])

(r/defquery all-people-names
  "Find all the people's names"
  []
  [?p :person/id _]
  [?p :person/name ?name])

(r/defsession base* 'factui.factui-test)

(def base (first (r/transact base* test-schema)))


(comment

  (require '[clara.rules :as cr])

  (def session (first (r/transact base [{:person/id 42
                                         :person/name "Luke"}
                                        {:person/id 88
                                         :person/name "Sam"}])))

  (cr/query session all-people-names)





  )

(deftest basic-txdata-and-queries
  (let [[s _] ]
    (is (= #{"Luke" "Sam"}
          (set (map :?name (r/query s all-people-names)))))



    )
  )





