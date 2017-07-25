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

(api/defquery person-name
  "Find a person by their name"
  [:find ?name
   :in ?pid
   :where
   [?p :person/name ?name]
   [?p :person/id ?pid]])

(api/rulebase rulebase factui.api-reactive-test)
(def base (api/session rulebase test-schema ::session))

#?(:clj
   (deftest simple-query

     (let [results (api/register ::session person-name [42])
           results2 (api/register ::no-such-session person-name [42])]

       (api/transact-all base [{:person/id 42
                                :person/name "Luke"}])

       (is (= #{["Luke"]} (a/<!! results)))

       (api/transact-all base [{:person/id 42
                                :person/name "John"}])

       (is (= nil (first (a/alts!! [(a/timeout 500) results2])))))

       ))

#?(:cljs
   (deftest simple-query
     (let [results (api/register ::session person-name [42])
           results2 (api/register ::no-such-session person-name [42])]

       (api/transact-all base [{:person/id 42
                                :person/name "Luke"}])

       (async done
         (go
           (is (= #{["Luke"]} (a/<! results)))

           (api/transact-all base [{:person/id 42
                                    :person/name "John"}])

           (is (= nil (first (a/alts! [(a/timeout 500) results2]))))

           (done))))))