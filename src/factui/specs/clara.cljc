(ns factui.specs.clara
  (:require [#?(:clj clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.string :as str]))

(s/def ::lhs (s/cat ::conditions (s/+ ::condition)))

(s/def ::condition
  (s/or ::boolean-expr ::boolean-expr
        ::accum-expr ::accum-expr
        ::fact-constraint ::fact-constraint
        ::test-expr ::test-expr))

(s/def ::boolean-expr (s/tuple #{:and :or :not} (s/or ::boolean-expr
                                                      ::fact-constraint)))

(s/def ::test-expr (s/tuple #{:test} ::s-expression))

(s/def ::s-expression list?)

(s/def ::accum-expr (s/tuple ::binding #{'<-} ::accumulator #{:from} ::fact-constraint))

(s/def ::binding-expr (s/cat ::binding ::binding ::arrow-symbol #{'<-}))

(s/def ::destructured-fact vector?)

(s/def ::fact-constraint (s/and vector?
                           (s/cat ::binding-expr (s/? ::binding-expr)
                                  ::fact-type any?
                                  ::destructured-fact (s/? ::destructured-fact)
                                  ::s-expressions (s/* ::s-expression))))

(s/def ::binding symbol?)
(s/def ::accumulator ifn?)

(s/def ::kw-query-var (s/and keyword?
                        #(str/starts-with? (name %) "?")))

(s/def ::defquery-args (s/cat ::name symbol?
                              ::docstr (s/? string?)
                              ::query-params (s/coll-of ::kw-query-var :kind vector?)
                              ::lhs ::lhs))

(s/def ::defrule-args (s/cat ::name symbol?
                             ::docstr (s/? string?)
                             ::lhs ::lhs
                             ::separator #{'=>}
                             ::rhs (s/+ any?)))