(ns factui.rum
  (:require [factui.api :as f]))

(defn- build
  [query-name query]
  `(do
     (f/defquery ~query-name ~query)

     {:will-mount #(on-update % ~query-name)

      :will-update #(on-update % ~query-name)

      :wrap-render (fn [render-fn#]
                     (fn [state# & render-args#]
                       (binding [*results* @(::results state#)]
                         (apply render-fn# state# render-args#))))

      :will-unmount (fn [state#]
                      (deregister state# ~query-name))}))

(defmacro q
  "Define a FactUI query, constructing a Rum mixin which will watch for
   changes to the given query, and re-render whenever the query results change.

   The following assumptions must hold, regarding the component:

   - The first argument to the component is an application state atom
   - The next N arguments are query inputs, where N is the number of inputs
   defined by the specified query.

   The component does not additional update semantics, but the FactUI mixin is
   fully composable with rum/static and rum/local."
  ([query]
   (build (gensym "factui-rum-query-") query))
  ([query-name query]
   (build query-name query)))