(ns factui.impl.compiler
  "Tools for converting Datalog expressions to Clara expressions"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clara.rules.compiler :as com]
            [clojure.walk :as w]
            [clojure.core.match :as m]
            [factui.specs.datalog :as ds]
            [factui.specs.clara :as cs]
            [factui.specs :as fs])
  (:refer-clojure :exclude [compile]))

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
  "Convert data pattern terms to a Clara fact constraint"
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
      [::cs/fact-constraint {::cs/fact-type (if (com/compiling-cljs?)
                                              'factui.facts/Datom
                                              'factui.facts.Datom)
                             ::cs/s-expressions exprs}])))

(defn- compile-boolean
  "Given an operation & set of clauses, emit a conformed Clara boolean
   constraint"
  [op clauses]
  [::cs/boolean-expr
   {::cs/operator op
    ::cs/exprs clauses}])

(defn- compile-maybe
  "Given a maybe clause, emit a Clara expression where either a set of options
   or its negation may be true."
  [clauses]
  (let [and [::cs/boolean-expr {::cs/operator :and
                                ::cs/exprs clauses}]]
    [::cs/boolean-expr
     {::cs/operator :or
      ::cs/exprs [and
                  [::cs/boolean-expr {::cs/operator :not
                                      ::cs/exprs [and]}]]}]))

(defn compile-defquery
  "Compile the arguments to a `defquery` form"
  [n]
  (m/match n

    ;; Populate name & docstr
    [::fs/name name] [::cs/name name]
    [::fs/docstr doc] [::cs/docstr doc]

    ;; normalize map & vector forms
    [::fs/vec-query vq] {::find (::ds/find vq)
                         ::in (-> vq ::ds/in-clause ::ds/in)
                         ::with (-> vq ::ds/with-clause ::ds/with)
                         ::where (-> vq ::fs/where-clause ::fs/where)}
    [::fs/map-query mq] {::find (:find mq)
                         ::in (:in mq)
                         ::with (:with mq)
                         ::where (:where mq)}

    ;; In clause
    {::fs/query {::in in}} #(-> %
                             (assoc ::cs/query-params (compile-in in))
                             (update ::fs/query dissoc ::in))

    ;; Lift conditions
    [::fs/clara-condition condition] condition
    [::fs/datomic-clause clause] clause

    ;; Build conditions
    [::ds/expression-clause [::ds/data-pattern {::ds/terms terms}]] (compile-constraint terms)

    ;; move where clauses to lhs conditions
    {::fs/query {::where where}} #(-> %
                                    (assoc-in [::cs/lhs ::cs/conditions] where)
                                    (update ::fs/query dissoc ::where))


    [::ds/or-clause {::ds/clauses clauses}] (compile-boolean :or clauses)
    [::ds/not-clause {::ds/clauses clauses}] (compile-boolean :not clauses)
    [::ds/and-clause {::ds/clauses clauses}] (compile-boolean :and clauses)

    [::ds/maybe-clause {::ds/clauses clauses}] (compile-maybe clauses)

    ;; find is temporarily unused
    [::find _] nil

    ;; with is temporarily unused
    [::with _] nil

    ;; Not-yet-implemented things

    [::ds/or-join-clause _] (throw (ex-info "'or-join' not yet implemented" {}))
    [::ds/not-join-clause _] (throw (ex-info "'not-join' not yet implemented" {}))
    [::ds/fn-expr _] (throw (ex-info "fn-expr not yet implemented" {}))
    [::ds/pred-expr _] (throw (ex-info "pred-expr not yet implemented" {}))
    [::ds/rule-expr _] (throw (ex-info "rule-expr not yet implemented" {}))

    :else n))

(defn compile-defrule
  "Compile the arguments to a `defrule` form"
  [n]
  (m/match n

    ;; Name and docstr
    ::fs/name ::cs/name
    ::fs/docstr ::cs/docstr
    ::fs/separator ::cs/separator

    ;; Lift conditions
    [::fs/clara-condition condition] condition
    [::fs/datomic-clause clause] clause

    ;; wrap in LHS
    [::fs/conditions conditions] [::cs/lhs {::cs/conditions conditions}]

    ::fs/rhs ::cs/rhs

    [::ds/or-clause {::ds/clauses clauses}] (compile-boolean :or clauses)
    [::ds/not-clause {::ds/clauses clauses}] (compile-boolean :not clauses)
    [::ds/and-clause {::ds/clauses clauses}] (compile-boolean :and clauses)

    [::ds/maybe-clause {::ds/clauses clauses}] (compile-maybe clauses)

    ;; Build conditions
    [::ds/expression-clause [::ds/data-pattern {::ds/terms terms}]] (compile-constraint terms)

    ;; Not-yet-implemented things
    [::ds/or-join-clause _] (throw (ex-info "'or-join' not yet implemented" {}))
    [::ds/not-join-clause _] (throw (ex-info "'not-join' not yet implemented" {}))
    [::ds/fn-expr _] (throw (ex-info "fn-expr not yet implemented" {}))
    [::ds/pred-expr _] (throw (ex-info "pred-expr not yet implemented" {}))
    [::ds/rule-expr _] (throw (ex-info "rule-expr not yet implemented" {}))

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

