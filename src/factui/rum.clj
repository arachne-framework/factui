(ns factui.rum
  (:require [factui.api :as f]))

(defn- build
  [query-name query]
  `(do
     (f/defquery ~query-name ~query)
     (mixin ~query-name)))

(defmacro q
  "Define a FactUI query and return a Rum mixin in a single step.

  Arguments are as for `factui.api/defquery`.

  See the docstring for `factui.rum/mixin` for details on the semantics of the
  resulting mixin."
  ([query]
   (build (gensym "factui-rum-query-") query))
  ([query-name query]
   (build query-name query)))