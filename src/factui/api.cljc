(ns factui.api
  (:require [factui.txdata :as txdata]
            [factui.rules :as r]
            [factui.facts :as f]
            [factui.compiler :as comp]
            [clara.rules :as cr]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def ^:no-doc bootstrap-schema
  "Initial schema for the FactUI fact base"
  [{:db/ident :db/valueType
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/unique
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/ident
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity
    :db/valueType :db.type/keyword}
   {:db/ident :db/cardinality
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/cardinality
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/keyword}
   {:db/ident :db/doc
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/string}
   {:db/ident :db/doc
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/string}
   {:db/ident :db/isComponent
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}
   ;; Compatibility purposes only
   {:db/ident :db/index
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}
   {:db/ident :db/fulltext
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}
   {:db/ident :db/noHistory
    :db/cardinality :db.cardinality/one
    :db/valueType :db.type/boolean}])

#?(:clj
   (defmacro defsession
     "Wrapper for Clara's `defsession`, which takes the same arguments.

    Automatically handles adding the FactUI schema & rules."
     [name & args]
     (let [original-name (gensym)]
       `(do
          (cr/defsession ~original-name 'factui.rules ~@args)
          (binding [r/*bootstrap* true]
            (def ~name
              (first (transact ~original-name bootstrap-schema))))))))

(defn transact
  "Add Datomic-style transaction data to the session, returning a tuple of the
   new session and a map of the tempid bindings."
  [session txdata]
  (binding [r/*tempid-bindings* (atom {})]
    (let [facts (txdata/txdata txdata)
          new-session (cr/fire-rules (cr/insert-all session facts))]
      [new-session @r/*tempid-bindings*])))

(defn transact-all
  "Apply multiple transactions sequentially, returning the updated session."
  [session & txes]
  (reduce (fn [s tx]
            (first (transact s tx)))
    session txes))

#?(:clj
   (defmacro defquery
     [name argvec query]
     `(cr/defquery ~name ~argvec
        ~@(comp/compile query))))

;; TODO: Test which of the following approaches is the most efficient:
;; 1. A Clara query, polled repeatedly
;; 2. A Clara rule that stores results (indexed by argument) in the fact store (or somewhere else)
;; 3. A Clara rule that pushes results to a channel (which can be filtered by argument).

