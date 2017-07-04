(ns factui.specs.clara
  (:require [#?(:clj clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.string :as str]))

(s/def ::lhs (s/coll-of ::condition :min-count 1))

(s/def ::condition
  (s/or :boolean-expr ::boolean-expr
        :accum-expr ::accum-expr
        :fact-constraint ::fact-constraint
        :test-expr ::test-expr))

(s/def ::boolean-expr (s/tuple #{:and :or :not} (s/or ::boolean-expr
                                                  ::fact-constraint)))

(s/def ::test-expr (s/tuple #{:test} ::s-expression))

(s/def ::s-expression list?)

(s/def ::accum-expr (s/tuple ::binding #{'<-} ::accumulator #{:from} ::fact-constraint))

(s/def ::binding-expr (s/cat :binding ::binding :arrow-symbol #{'<-}))

(s/def ::destructured-fact vector?)

(s/def ::fact-constraint (s/and vector?
                           (s/cat :binding-expr (s/? ::binding-expr)
                             :fact-type symbol?
                             :destructured-fact (s/? ::destructured-fact)
                             :s-expressions (s/* ::s-expression))))

(s/def ::binding symbol?)
(s/def ::accumulator ifn?)


(comment

  (def d
    '[[factui.facts.Datom (= e ?e) (= a :x/x)]
      [factui.facts.Datom (= e ?e) (= a :x/y)]

      ])

  (use 'clojure.pprint)

  (pprint
    (s/conform ::lhs d))
  )

