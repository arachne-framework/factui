(ns factui.bench.basic-rules
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


(cr/defrule person-book-topics
  "If a person has read a book on a topic, they must like this topic (why not)"
  [:person/has-read [{:keys [e v]}] (= e ?p) (= v ?b)]
  [:book/topic [{:keys [e v]}] (= e ?b) (= v ?topic)]
  =>
  (api/transact! [{:db/id ?p
                   :person/likes ?topic}]))

(cr/defrule people-make-friends-from-books
  "If two people have read the same book, they must be friends (wat)"
  [:person/has-read [{:keys [e v]}] (= e ?p1) (= v ?b)]
  [:person/has-read [{:keys [e v]}] (= e ?p2) (= v ?b)]
  [:test (not= ?p1 ?p2)]
  =>
  (api/transact! [{:db/id ?p1
                   :person/friends ?p2}]))

(cr/defrule friends-are-always-mutual
  "Friends are mutual"
  [:person/friends [{:keys [e v]}] (= e ?p1) (= v ?p2)]
  [:not [:person/friends [{:keys [e v]}] (= e ?p2) (= v ?p1)]]
  =>
  (api/transact! [{:db/id ?p2
                   :person/friends ?p1}]))

(cr/defquery person-by-id
  [:?pid]
  [:person/id [{:keys [e v]}] (= e ?p) (= v ?pid)]
  [:person/name [{:keys [e v]}] (= e ?p) (= v ?name)]
  [:person/age [{:keys [e v]}] (= e ?p) (= v ?age)]
  [:person/friends [{:keys [e v]}] (= e ?p) (= v ?friends)]
  [:person/likes [{:keys [e v]}] (= e ?p) (= v ?likes)]
  [:person/reading [{:keys [e v]}] (= e ?p) (= v ?reading)]
  [:person/has-read [{:keys [e v]}] (= e ?p) (= v ?has-read)])

(cr/defquery book-by-id
  [:?pid]
  [:book/id [{:keys [e v]}] (= e ?p) (= v ?pid)]
  [:book/title [{:keys [e v]}] (= e ?p) (= v ?title)]
  [:book/topic [{:keys [e v]}] (= e ?p) (= v ?topic)])

(cr/defquery person-friends
  []
  [:person/friends [{:keys [e v]}] (= e ?p) (= v ?p2)])

(cr/defquery dum-datoms
  []
  [?d <- Datom])

