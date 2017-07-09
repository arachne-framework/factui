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
  [Datom (= e ?p) (= a :person/has-read) (= v ?b)]
  [Datom (= e ?b) (= a :book/topic) (= v ?topic)]
  =>
  (api/transact! {:db/id ?p
                  :person/likes ?topic}))

(cr/defrule people-make-friends-from-books
  "If two people have read the same book, they must be friends (wat)"
  [Datom (= e ?p1) (= a :person/has-read) (= v ?b)]
;  [Datom (= e ?p2) (= a :person/has-read) (= v ?b)]
;  [:test (not= ?p1 ?p2)]
  =>
  (println ?p1 ?b)
  #_(api/transact! {:db/id ?p1
                  :person/friends ?p2}))

(cr/defrule friends-are-always-mutual
  "Friends are mutual"
  [Datom (= e ?p1) (= a :person/friends) (= v ?p2)]
  [:not [Datom (= e ?p2) (= a :person/friends) (= v ?p1)]]
  =>
  (api/transact! {:db/id ?p2
                  :person/friends ?p1}))

(cr/defquery person-by-id
  [:?pid]
  [Datom (= e ?p) (= a :person/id) (= v ?pid)]
  [Datom (= e ?p) (= a ?a) (= v ?v)])

(cr/defquery book-by-id
  [:?pid]
  [Datom (= e ?p) (= a :book/id) (= v ?pid)]
  [Datom (= e ?p) (= a ?a) (= v ?v)])

(cr/defquery person-friends
  []
  [Datom (= e ?p) (= a :person/friends) (= v ?p2)])

