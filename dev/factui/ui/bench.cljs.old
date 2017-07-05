(ns factui.ui.bench
  (:require [rum.core :as rum]
            [factui.api :as fui]
            [factui.rules :as fools]
            [cljs.core.async :as a]
            [cljs.pprint :refer [pprint]])
  (:require-macros [factui.api :as fui]))

(enable-console-print!)

(defn now []
  (.getTime (js/Date.)))

(defn rand-date []
  (js/Date. (- (now) (* 1000 60 60 24 365 (inc (rand-int 100))))))

(def schema
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
   {:db/ident :person/reads
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many}
   {:db/ident :person/age
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :person/dob
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident :person/friends
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/many}

   {:db/ident :book/id
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :book/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :book/subject
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many}])

(defn rand-string
  []
  (let [len (+ 5 (rand-int 20))]
    (apply str (repeatedly len
                 #(rand-nth "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))))

(defn rand-person
  [id]
  {:person/id id
   :person/name (rand-string)
   :person/dob (rand-date)
   :person/likes (repeatedly (inc (rand-int 10)) rand-string)})

(defn rand-book
  [id]
  {:book/id id
   :book/title (rand-string)
   :book/subject (repeatedly (inc (rand-int 5)) rand-string)})

(fui/defsession *base 'factui.ui.bench)

;(def base (fui/transact-all base* schema))


(defn ^:export main
  "Initialize the app and start rendering"
  []
  (println "hello, world!")


  )