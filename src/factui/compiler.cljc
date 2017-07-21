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

    ;; Clean up empty query
    ;[::fs/query {}] nil

    ;; find is temporarily unused
    [::find _] nil

    ;; with is temporarily unused
    [::with _] nil

    ;; Not-yet-implemented things
    [::ds/or-clause _] (throw (ex-info "'or' not yet implemented" {}))
    [::ds/or-join-clause _] (throw (ex-info "'or-join' not yet implemented" {}))
    [::ds/not-clause _] (throw (ex-info "'not' not yet implemented" {}))
    [::ds/not-join-clause _] (throw (ex-info "'not-join' not yet implemented" {}))
    [::ds/fn-expr _] (throw (ex-info "fn-expr not yet implemented" {}))
    [::ds/pred-expr _] (throw (ex-info "pred-expr not yet implemented" {}))
    [::ds/rule-expr _] (throw (ex-info "rule-expr not yet implemented" {}))

    :else n))

(comment

  (def c (s/conform ::fs/defquery-args
           '(person-name
              "find a person"
              [:find ?name
               :in ?id
               :where [?id :person/name ?name]])))

  (def r
    '{:factui.specs.clara/name person-name,
      :factui.specs.clara/docstr "find a person",
      :factui.specs/query {},
      :factui.specs.clara/query-params (:?id),
      :factui.specs.clara/lhs
      {:factui.specs.clara/conditions
       [:factui.specs.clara/fact-constraint
        {:factui.specs.clara/fact-type :person/name,
         :factui.specs.clara/destructured-fact [{:keys [e v]}],
         :factui.specs.clara/s-expressions ((= e ?id) (= v ?name))}]}})

  (def c (s/conform ::cs/defquery-args
           '(person-name
              "find a person"
              [:?x]
              [:fact-type [{:keys [e v]}] (= e ?id) (= v ?name)]

              )
           ))

  (clojure.pprint/pprint c)

  (s/unform ::cs/defquery-args r)



  (factui.api/defquery person-name
    "find a person"
    [:find ?name
     :in ?id
     :where [?id :person/name ?name]])

  )

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