(ns factui.repro-test
  (:require
   #?(:cljs [factui.api :as f :include-macros true]
      :clj [factui.api :as f])
   #?(:clj
           [clara.rules :as cr]
      :cljs [clara.rules :as cr :include-macros true])
           [clojure.pprint :refer [pprint]]
   #?(:clj
           [clojure.test :as t :refer [deftest is testing run-tests]]
      :cljs [cljs.test :as t :refer-macros [deftest is testing run-tests]])

   #?(:cljs [factui.facts :as fact :refer [Datom]] :clj [factui.facts :as fact]))

  #?(:clj (:import [factui.facts Datom])))


(def schema
  [{:db/ident :task/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :task/completed
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(f/defquery task-q
  [:find [?title ?completed]
   :in ?t
   :where
   [?t :task/title ?title]
   [?t :task/completed ?completed]])

(f/defsession base ['factui.repro-test] schema)

(def initial-data
  [{:db/id -42
    :task/title "Task A"
    :task/completed "a"}])

(deftest simple-rule
  (let [[s1 bindings] (f/transact base initial-data)
        eid (bindings -42)
        result1 (f/query-raw s1 task-q eid)]
    (is (= (set result1)
          #{{:?t eid, :?title "Task A", :?completed "a"}}))

    (let [[s2 _] (f/transact s1 [{:db/id eid
                                  :task/completed "b"}])
          result2 (f/query-raw s2 task-q eid)]

      ;; TODO: The retraction of "a" is sent to Clara, but doesn't seem to take effect

      (is (= (set result1)
            #{{:?t eid, :?title "Task A", :?completed "b"}}))


      (let [[s3 _] (f/transact s2 [{:db/id eid
                                    :task/completed "c"}])
            result3 (f/query-raw s3 task-q eid)]

        ;; TODO: But the retraction of "b" does!

        (is (= (set result1)
              #{{:?t eid, :?title "Task A", :?completed "c"}}))

        )

      )

    ))


