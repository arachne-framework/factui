(ns factui.specs
  (:require [#?(:clj clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [factui.specs.datalog :as ds]
            [clojure.string :as str]))

(s/def ::defquery-args
  (s/cat ::name symbol?
         ::docstr (s/? string?)
         ::query ::ds/query))

(comment

  (use 'clojure.pprint)

  (pprint
    (s/conform ::defquery-args
      '(find-person "docstr"
         [:find ?x
          :in ?y
          :where
           [?y :person/name ?x]])))

  )

