(ns factui.bench
  (:require
    [factui.api :as api :include-macros true]
    [factui.facts :as f]
    [clojure.pprint :refer [pprint]]
    #?(:clj [clojure.pprint :refer [pprint]]
       :cljs [cljs.pprint :refer [pprint]])))

#?(:cljs (enable-console-print!))

(defn now []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

(defn rand-date []
  (let [ts (- (now) (* 1000 60 60 24 365 (inc (rand-int 100))))]
    #?(:cljs (js/Date. ts)
       :clj (java.util.Date. ts))))

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

(api/defsession base ['factui.bench] schema)

(def num-people 50)
(def num-books 50)

(defn count-thing [d]
  (cond
    (map? d) (reduce + 1 (map count-thing (vals d)))
    (coll? d) (reduce + 0 (map count-thing d))
    :else 1))

(defn estimate-datoms
  "Estimate the number of datoms in the given txdata"
  [txdata]
  (reduce + 0 (map count-thing txdata)))

(defn gen-data []
  (doall
    (concat
      (for [pid (range 0 num-people)]
        (rand-person pid))
      (for [pid (range 0 num-books)]
        (rand-book pid)))))

(defn time-per-datom [num-datoms f]
  (let [start (now)]
    (f)
    (let [stop (now)
           elapsed (- stop start)
           by-datom (* 1.0 (/ elapsed num-datoms))]
       (println "Did" num-datoms "in" elapsed "ms, for" by-datom "ms per datom"))))

(defn -main [& _]
  (let [txdata (gen-data)
        n (estimate-datoms txdata)
        f #(api/transact base txdata)]
    (println "Generated about" n "datoms")
    (println "Warming up....")
    (dotimes [i 100] (f))
    (println "Benchmarking...")
    (time-per-datom n f)))

;; Run tests at the root level, in CLJS
#?(:cljs
   (do (-main)
       (.exit js/phantom)))
