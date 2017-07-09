(ns factui.bench
  (:require
    [factui.api :as api :include-macros true]
    [factui.facts :as f]
    [factui.bench.basic-rules :as br]
    [clojure.pprint :refer [pprint]]
    #?(:clj [clojure.pprint :refer [pprint]]
       :cljs [cljs.pprint :refer [pprint]])
   #?(:clj [clara.rules :as cr]
      :cljs [clara.rules :as cr :include-macros true])
    ))

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
   {:db/ident :person/reading
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :person/has-read
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}

   {:db/ident :book/id
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :book/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :book/topic
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many}])

(defn rand-string
  []
  (let [len (+ 5 (rand-int 20))]
    (apply str (repeatedly len
                 #(rand-nth "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))))

(defn mkeep
  [m]
  (into {} (filter (fn [[k v]]
                     (and v (or (not (coll? v))
                                (not (empty? v))))) m)))

(defn rand-person
  [id people-range book-range]
  (mkeep
    {:person/id id
     :person/name (rand-string)
     :person/dob (rand-date)
     :person/likes (repeatedly (inc (rand-int 2)) rand-string)
     :person/reading {:book/id (rand-nth book-range)}
     :person/has-read (repeatedly (rand-int 2)
                        (fn []
                          {:book/id (rand-nth book-range)}))}))

(defn rand-book
  [id book-range]
  {:book/id id
   :book/title (rand-string)
   :book/topic (repeatedly (inc (rand-int 5)) rand-string)})

(api/defsession base ['factui.bench.basic-rules] schema)

(def num-people 1000)
(def num-books 100)

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
  (let [people-range (range 0 num-people)
        book-range (range 0 num-books)]
    (doall
      (concat
        (for [pid people-range]
          (rand-person pid people-range book-range))
        (for [pid (range 0 num-books)]
          (rand-book pid book-range))))))

(defn time-per-datom [num-datoms f]
  (let [start (now)]
    (f)
    (let [stop (now)
           elapsed (- stop start)
           by-datom (* 1.0 (/ elapsed num-datoms))]
       (println "Did" num-datoms "in" elapsed "ms, for" by-datom "ms per datom"))))

(defn -main [& [num-people num-books]]
  (let [txdata (gen-data)
        n (estimate-datoms txdata)
        f #(api/transact base txdata)]
    (println "Generated about" n "datoms")
    (println "Warming up....")
    (dotimes [i 5] (f))
    (println "Benchmarking...")
    (time-per-datom n f)))


(comment

  (def txdata (gen-data))

  (pprint txdata)

  (time
    (def s (api/transact-all base txdata)))

  (pprint
    (cr/query s br/dum-datoms))

  )

;; Run tests at the root level, in CLJS
#?(:cljs
   (do (-main)
       (.exit js/phantom)))
