(ns factui.bench
  (:require
    [factui.api :as api :include-macros true]
           [factui.facts :as f]
    [clara.rules :as cr :include-macros true]
    [factui.rules :as r :include-macros true]
    [clojure.pprint :refer [pprint]]
    [clara.rules.memory :as mem]
    [clara.tools.tracing :as ct]
    [cljs.pprint :refer [pprint]]
    ))

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

(api/defsession base* 'factui.bench)

(def base (api/transact-all base* schema))

(def num-people 20)

(def num-books 0)

(println "Generating txdata...")

(def txdata
  (doall
    (concat
      (for [pid (range 0 num-people)]
        (rand-person pid))
      (for [pid (range 0 num-books)]
        (rand-book pid)))))

(println "Warming up....")

(dotimes [n 5]
  (api/transact-all base txdata))

(println "Benchmarking...")
(println "(There are" (count txdata) "entities)")

(time
  (api/transact-all base txdata))


(def tracing-session (ct/with-tracing base))

(println "Trace:")
(pprint (ct/get-trace (api/transact-all tracing-session [{:book/id 42
                                                          :book/title "My Book"}])))

(println "version:" (.-appVersion js/navigator))


(.exit js/phantom)

