(ns factui.compiler
  "Tools for converting Datalog expressions to Clara expressions"
  (:require [#?(:clj clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.string :as str]
            [clojure.walk :as w]
            [factui.specs.datalog :as ds]
            [factui.specs.clara :as cs])
  (:refer-clojure :exclude [compile]))

(defn- tagged-value?
  [v]
  (and (vector? v) (= 2 (count v)) (keyword? (first v))))

(defmulti transform
  "Applies a transform to each node of the parse tree. Default behavior is to
   pass through unchanged."
  (fn [node]
    (if (tagged-value? node)
      (first node)
      ::default))
  :default ::default)

(defmethod transform ::default [form] form)

(defmethod transform :vec-query
  [[_ q]]
  [:clauses
   [[:find (-> q :find-spec)]
    [:where-clauses (-> q :where-clause :clauses)]]])

(defmethod transform :map-query
  [[_ q]]
  [:clauses
   [[:find (-> q :find)]
    [:where-clauses (-> q :where)]]])

(defmethod transform :clauses [[_ clauses]]
  (apply concat clauses))

#_(s/def ::accum-expr (s/tuple ::binding #{'<-} ::accumulator #{:from} ::fact-constraint))

(defmethod transform :find [[_ f]]
  [f]
  [
   ;; TODO: Find with pull will need to insert some clauses
   ;; Otherwise does not affect clauses
   #_[:accum-expr ['?find-results
                 '<-
                 `(acc/find)
                 :from
                 {:fact-type 'factui.facts.Datom
                  :s-expressions ['(= e ?e)]}]]])

(defmethod transform :where-clauses
  [[_ clauses]]
  clauses)

(defmethod transform :expression-clause
  [[_type clause]]
  clause)

(defmethod transform :data-pattern
  [[_ {terms :terms} :as dp]]
  (let [exprs (filter identity
                (map (fn [pos [type term]]
                       (when (and term (not= type :placeholder))
                         `(~'= ~pos ~term)))
                  '[e a v]
                  terms))]
    [:fact-constraint {:fact-type 'factui.facts.Datom
                       :s-expressions exprs}]))

(defn compile
  "Given data that conforms to the spec for a Datalog query, emit data that
   conforms to the spec for a Clara LHS."
  [query]
  (let [input (s/conform ::ds/query query)
        output (loop [in input]
                 (let [out (w/prewalk transform in)]
                   (if (= in out)
                     out
                     (recur out))))]
    (s/unform ::cs/lhs output)))
