(ns factui.ui.bench
  (:require [rum.core :as rum]
            [clara.rules :as r :refer-macros [defquery defrule]]
            ;[factui.rules :as fui :refer-macros [defrule #_defquery]]
            [cljs.core.async :as a]
            [cljs.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defrecord Datom [e a v])

(def attributes [:person/first-name :person/last-name :person/age :entity/id
                 :entity/meta :entity/location :entity/size
                 :meta/a :meta/b :meta/c
                 :gamma/x :gamma/y :gamma/z :delta/x :delta/y :delta/z])

(defn rand-string
  []
  (let [len (+ 5 (rand-int 20))]
    (apply str (repeatedly len
                 #(rand-nth "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))))

;; comment
(let [next (atom 10000)]
  (defn next-eid []
    (swap! next inc)
    @next))

(defn rand-entity
  [entity-size attrs]
  (let [eid (next-eid)]
    (repeatedly entity-size #(->Datom eid (rand-nth attrs) (rand-string)))))

(defn generate [entity-size attr-count entity-count]
  "Return a set of randomly generated Datoms"
  (let [attrs (vec (take attr-count attributes))]
    (apply concat (repeatedly entity-count #(rand-entity entity-size attrs)))))

(defn now []
  (.getTime (js/Date.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule full-names
  "Calculate the speed"
  [Datom (= a :person/first-name) (= e ?e) (= v ?first)]
  [Datom (= a :person/last-name) (= e ?e) (= v ?last)]
  =>
  (let [fullname (str ?first " " ?last)]
    (r/insert! (->Datom ?e :person/full-name fullname))))

(defrule multiple-names
  "Calculate the speed"
  [Datom (= a :person/full-name) (= e ?e) (= v ?name1)]
  [Datom (= a :person/full-name) (= e ?e) (= v ?name2)]
  [:test (not= ?name1 ?name2)]
  =>
  (r/insert! (->Datom ?e :person/has-multiple-names true)))

(defquery basic-query
  "Query to find an entity"
  [:?eid]
  [?datom <- Datom (= ?eid e)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(r/defsession blank-session 'factui.ui.bench)


(defonce datoms (do
                  (println "generating datoms")
                  (time
                    (doall
                      (generate 10 10 10000)))))

#_(def datoms [(->Datom 42 :person/first-name "Luke")
             (->Datom 42 :person/last-name "VanderHart")])

(defn basics
  []
  (-> blank-session
    (r/insert-all datoms)
    (r/fire-rules)))

(def session (atom blank-session))

(defn populate-session
  "Return a populated session"
  []
  (println "inserting:" (count datoms))
  (time
    (reset! session (-> blank-session
                      (r/insert-all datoms)
                      (r/fire-rules)))))

(defn q1
  []
  (vec
    (time
      (r/query @session basic-query :?eid 10001)))

  )

(defn qbench
  []
  (time
    (dotimes [i 300]
      (r/query @session basic-query :?eid (+ 10000 i)))
    )
  )


(defn ^:export main
  "Initialize the app and start rendering"
  []
  ;(bench 1000)
  )