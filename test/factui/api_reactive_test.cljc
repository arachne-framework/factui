(ns factui.api-reactive-test
  (:require

   #?(:cljs [factui.api :as api :include-macros true]
      :clj [factui.api :as api])
   #?(:clj
      [clara.rules :as cr]
      :cljs [clara.rules :as cr :include-macros true])
      [clojure.pprint :refer [pprint]]
   #?(:clj [clojure.test :as t :refer [deftest is testing run-tests]]
      :cljs [cljs.test :as t :refer-macros [deftest is testing run-tests async]])

   #?(:cljs [factui.facts :as f :refer [Datom]] :clj [factui.facts :as f])

   #?(:clj [clojure.core.async :as a]
      :cljs [cljs.core.async :as a]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]]))
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

(api/defquery person-age
  "Find a person and return their age."
  [:find ?age
   :in ?pid
   :where
   [?p :person/age ?age]
   [?p :person/id ?pid]])

(api/defquery person-name
  "Find a person and return their name"
  [:find ?name
   :in ?pid
   :where
   [?p :person/name ?name]
   [?p :person/id ?pid]])


(api/defquery all-names
  "Find all people and names"
  [:find ?p ?name
   :where
   [?p :person/name ?name]])

(api/rulebase rulebase factui.api-reactive-test)
(def base (api/session rulebase test-schema))

#?(:clj
   (deftest simple-query

     (let [name-results (api/register (:session-id base) person-name [42])
           age-results (api/register (:session-id base) person-age [42])
           never-results (api/register "no-such-session" person-name [42])]

       (api/transact-all base [{:person/id 42
                                :person/age 32
                                :person/name "Luke"}])

       (is (= #{["Luke"]} (first (a/alts!! [name-results (a/timeout 500)]))))
       (is (= #{[32]} (first (a/alts!! [age-results (a/timeout 500)]))))

       (api/transact-all base [{:person/id 42
                                :person/age 18
                                :person/name "John"}])

       (is (= nil (first (a/alts!! [(a/timeout 500) never-results])))))

       ))

#?(:cljs
   (deftest simple-query
     (let [name-results (api/register (:session-id base) person-name [42])
           age-results (api/register (:session-id base) person-age [42])
           never-results (api/register "no-such-session" person-name [42])]

       (api/transact-all base [{:person/id 42
                                :person/age 32
                                :person/name "Luke"}])

       (async done
         (go
           (is (= #{["Luke"]} (first (a/alts! [(a/timeout 500) name-results]))))
           (is (= #{[32]} (first (a/alts! [(a/timeout 500) age-results]))))

           (api/transact-all base [{:person/id 42
                                    :person/age 18
                                    :person/name "John"}])

           (is (= nil (first (a/alts! [(a/timeout 500) never-results]))))

           (done))))))

#?(:clj
   (deftest retractions-are-reactive

     (let [results (api/register (:session-id base) all-names [])
           [s1 bindings] (api/transact base [{:db/id -42
                                              :person/id 42
                                              :person/name "Luke"}])
           eid (bindings -42)]

       (is (= (api/query s1 all-names)
              (first (a/alts!! [results (a/timeout 500)]))))

       (let [[s2 _] (api/transact s1 [{:person/id 42
                                       :person/name "Luke V."}
                                      {:person/id 99
                                       :person/name "Joe"}])]

         (is (= (api/query s2 all-names)
                (first (a/alts!! [results (a/timeout 500)]))))

         (let [[s3 _] (api/transact s2 [[:db/retract eid :person/name "Luke V."]])]

           (is (= (api/query s3 all-names)
                  (first (a/alts!! [results (a/timeout 2000)])))))))))