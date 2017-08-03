(ns factui.api-reactive-test
  (:require

   #?(:cljs [factui.api :as api :include-macros true]
      :clj [factui.api :as api])
   #?(:clj [clara.rules.compiler :as com])
   #?(:clj
      [clara.rules :as cr]
      :cljs [clara.rules :as cr :include-macros true])
      [clojure.pprint :refer [pprint]]
   #?(:clj [clojure.test :as t :refer [deftest is testing run-tests]]
      :cljs [cljs.test :as t :refer-macros [deftest is testing run-tests async]])

   #?(:cljs [factui.facts :as f :refer [Datom]] :clj [factui.facts :as f])

   #?(:clj [clojure.core.async :as a :refer [go go-loop]]
      :cljs [cljs.core.async :as a]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                            [factui.api-reactive-test :refer [async-body]]))
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
    :db/cardinality :db.cardinality/many}
   {:db/ident :person/nicknames
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many}
   ])

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

(api/defquery all-ages
  "Find all people w ages"
  [:find ?p ?age
   :where
   [?p :person/age ?age]])

(api/rulebase rulebase factui.api-reactive-test)
(def base (api/session rulebase test-schema))

(defn- wait-for
  [ch]
  (go (first (a/alts! [(a/timeout 500) ch]))))

#?(:clj (defmacro async-body [& body]
          (if (com/compiling-cljs?)
            `(cljs.test/async done#
               (cljs.core.async.macros/go
                 ~@body
                 (done#)))
            `(clojure.core.async/<!!
               (clojure.core.async/go
                 ~@body)))))

(deftest simple-query

   (let [name-ch (a/chan)
         age-ch (a/chan)
         never-ch (a/chan)
         s (atom base)]

     (swap! s api/register person-name [42] name-ch)
     (swap! s api/register person-age [42] age-ch)
     (swap! s api/register person-name [666] never-ch)

     (swap! s api/transact-all [{:db/id 1042
                                 :person/id 42
                                 :person/age 32
                                 :person/name "Luke"}])

     (async-body

       (is (= #{["Luke"]} (a/<! (wait-for name-ch))))
       (is (= #{[32]} (a/<! (wait-for age-ch))))
       (is (nil? (a/<! (wait-for never-ch))))

       (testing "updates re-trigger the query"
         (swap! s api/transact-all [{:db/id 1042 :person/age 33}])
         (is (= #{[33]} (a/<! (wait-for age-ch)))))

       (testing "retractions trigger an update"
         (swap! s api/transact-all [[:db/retract 1042 :person/name "Luke"]])
         (is (= #{} (a/<! (wait-for name-ch))))))))