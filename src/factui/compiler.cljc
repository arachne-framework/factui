(ns factui.compiler
  "Tools for converting Datalog expressions to Clara expressions"
  (:require [#?(:clj clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.string :as str]
            [clojure.walk :as w]
            #?(:clj [clojure.core.match :as m]
               :cljs [cljs.core.match :as m :include-macros true])
            [factui.specs.datalog :as ds]
            [factui.specs.clara :as cs]
            [factui.specs :as fs])
  (:refer-clojure :exclude [compile]))

(defn mkeep [m]
  "Remove nil/empty values from map"
  (into {} (filter (fn [[_ v]]
                     (if (coll? v)
                       (not-empty v)
                       v))
             m)))

(defn- compile-in
  "Compile a ::ds/in to a ::cs/query-params"
  [in]
  (for [clause in]
    (m/match clause
      [::ds/binding [::ds/bind-scalar v]] (keyword v)
      [::ds/binding _] (throw (ex-info "Only scalar bindings for :in clauses are currently supported." {}))
      [::ds/rules-var] (throw (ex-info "Datalog rules are not supported." {}))
      [::ds/src-var (throw (ex-info "Datalog sources are not supported." {}))])))

(defn- compile-constraint
  "Convert data pattern terms toa Clara fact constraint"
  [[[e-tag e-form]
    [a-tag a-form]
    [v-tag v-form]]]
  (let [e-expr (when (not= ::ds/placeholder e-tag)
                 (list '= 'e e-form))
        a-expr (when (= ::ds/variable a-tag)
                 (list '= 'a a-form))
        v-expr (when (not= ::ds/placeholder v-tag)
                 (list '= 'v v-form))
        exprs (filter identity [e-expr a-expr v-expr])]
    (if (= ::ds/constant a-tag)
      [::cs/fact-constraint {::cs/fact-type a-form
                             ::cs/destructured-fact '[{:keys [e v]}]
                             ::cs/s-expressions exprs}]
      [::cs/fact-constraint {::cs/fact-type 'factui.facts.Datom
                             ::cs/s-expressions exprs}])))

(defn compile-defquery
  "Compile the arguments to a `defquery` form"
  [n]
  (m/match n

    ;; normalize map & vector forms
    [::ds/vec-query vq] (mkeep
                          {::find (::ds/find vq)
                           ::in (-> vq ::ds/in-clause ::ds/in)
                           ::with (-> vq ::ds/with-clause ::ds/with)
                           ::where (-> vq ::ds/where-clause ::ds/where)})
    [::ds/map-query mq] (mkeep
                          {::find (:find mq)
                           ::in (:in mq)
                           ::with (:with mq)
                           ::where (:where mq)})


    ;; Populate name & docstr
    [::fs/name name] [::cs/name name]
    [::fs/docstr doc] [::cs/docstr doc]

    ;; In clause
    {::fs/query {::in in}} #(-> %
                             (assoc ::cs/query-params (compile-in in))
                             (update ::fs/query dissoc ::in))

    ;; Build conditions
    [::ds/expression-clause [::ds/data-pattern {::ds/terms terms}]] (compile-constraint terms)


    ;; move where clauses to lhs conditions
    {::fs/query {::where where}} #(-> %
                                    (assoc-in [::cs/lhs ::cs/conditions] where)
                                    (update ::fs/query dissoc ::where))
    ;; find is temporarily unused
    [::find _] nil

    :else n))

(defn compile
  "Convert the input data to output data by walking each node of the input
   using `clojure.walk/prewalk` and applying the supplied compiler function.
   Compilation is finished when this leaves the data unchanged.

   For each node, the compiler function may return either a function or a
   value, which will be used to calculate (or simply replace) the node."
  [in compiler]
  (let [out (w/prewalk (fn [node]
                         (let [v (compiler node)]
                           (if (fn? v) (v node) v)))
              in)]
    (if (= in out)
      out
      (recur out compiler))))