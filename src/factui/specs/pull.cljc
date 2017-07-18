(ns factui.specs.pull
  (:require [#?(:clj clojure.spec.alpha
                :cljs cljs.spec.alpha) :as s]
            [clojure.string :as str]))

(s/def ::pattern (s/coll-of ::attr-spec :min-count 1))

(s/def ::attr-spec (s/or ::attr-name ::attr-name
                         ::wildcard ::wildcard
                         ::map-spec ::map-spec
                         ::attr-expr ::attr-expr))

(s/def ::attr-name qualified-keyword?)

(s/def ::wildcard #{"*" '*})

(s/def ::map-spec (s/map-of (s/or ::attr-name ::attr-name
                                  ::limit-expr ::limit-expr)
                            (s/or ::pattern ::pattern
                                  ::recursion-limit ::recursion-limit)))


(s/def ::limit-expr (s/cat ::limit #{"limit" 'limit}
                           ::attr-name ::attr-name
                           ::max (s/or ::positive-number pos-int?
                                       ::nil nil?)))

(s/def ::default-expr (s/cat ::default #{"default" 'default}
                             ::attr-name ::attr-name
                             ::value any?))

(s/def ::attr-expr (s/or ::limit-expr ::limit-expr
                         ::default-expr ::default-expr))

(s/def ::recursion-limit (s/or ::positive-number pos-int?
                               ::ellipses #{'...}))