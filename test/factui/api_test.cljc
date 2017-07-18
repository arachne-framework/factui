(ns factui.api-test
  (:require
    #?(:cljs [factui.api :as api :include-macros true]
       :clj [factui.api :as api])
   #?(:clj [clara.rules :as cr]
      :cljs [clara.rules :as cr :include-macros true])
    [clojure.pprint :refer [pprint]]
   #?(:clj [clojure.test :as t :refer [deftest is testing run-tests]]
      :cljs [cljs.test :as t :refer-macros [deftest is testing run-tests]])))

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

(api/defquery person-name-map
  "Find a person by their name"
  {:find [?name]
   :in [?id]
   :where [[?id :person/name ?name]]})

(api/defsession base ['factui.api-test] test-schema)

(deftest simple-query
  (let [[s bindings] (api/transact base [{:db/id -99
                                          :person/name "Luke"}
                                         {:db/id -100
                                          :person/name "Alex"}])
        luke-id (bindings -99)
        alex-id (bindings -100)
        result (api/query s person-name luke-id)
        result-m (api/query s person-name-map luke-id)]
    (is (= result result-m))
    (is (= 1 (count result)))))
