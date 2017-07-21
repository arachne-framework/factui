(ns factui.specs
  (:require [#?(:clj clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [factui.specs.datalog :as ds]
            [factui.specs.clara :as cs]
            [clojure.string :as str]))

(s/def ::condition (s/or ::datomic-clause ::ds/clause
                         ::clara-condition ::cs/condition))

(s/def ::where (s/+ ::condition))

(s/def ::query (s/or ::map-query ::map-query
                     ::vec-query ::vec-query))

(s/def ::map-query (s/keys :req-un [::ds/find ::where]
                           :opt-un [::ds/with ::ds/in]))

(s/def ::vec-query
  (s/cat ::ds/find-literal #{:find}
         ::ds/find ::ds/find
         ::ds/with-clause (s/? (s/cat ::ds/with-literal #{:with} ::ds/with ::ds/with))
         ::ds/in-clause (s/? (s/cat ::ds/in-literal #{:in} ::ds/in ::ds/in))
         ::where-clause (s/? (s/cat ::where-literal #{:where} ::where ::where))))

(s/def ::defquery-args
  (s/cat ::name symbol?
         ::docstr (s/? string?)
         ::query ::query))

(s/def ::defrule-args
  (s/cat ::name symbol?
         ::docstr (s/? string?)
         ::conditions (s/+ ::condition)
         ::separator #{'=>}
         ::rhs (s/+ any?)))