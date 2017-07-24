(ns factui.rum
  (:require [rum.core :as rum]
            [factui.api :as f :include-macros true]
            [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

;; DONE: Allow non-query args (& validate that they correctly force an update)
;; DONE: Test figwheel forced updates

;; TODO: Add pulse predicates, create demo of event handling


;; How to make it faster:

;; The expensive part is definitely the N queries, where N is the number of
;; components. Although each query is pretty quick (~0.1ms), doing that many
;; queries and comparisons adds up (for about 100ms with 2k components)

;; IDEAS:
;; - Don't use for "live loop" animations
;; - Move events & reactions to web worker

;; - Use a Clara rule instead of a Clara query for reactiveness. Still have to do the same amount of logical work (matching updates to components) but can optimize the algorithm, and, most importantly, avoid the heavyweight comparisons
;; - Downside:: need to map "many results"

;; Current algo: O(n) where n is the number of FactUI components. Each operation also incurs the cost of a (potentially expensive) data structure comparison.

;; Algo A: Have a stateful rule AND a query. Components register with the
;; stateful rule. When the rule fires, fire a "rerender" notification to all the components for which
;; it is a match (components whose args match!). But: key point: we can use a hashtable for this instead
;; of a linear list of components, giving us O(1) instead of O(N) performance.

;; Algo B: ONly a rule. The rule is smart enough to aggregate all "hits"
;; into a single result set, and notifies the correct component based on that.

;; TODO: prove concept
;; TODO: Write "reactive-query" macro (to combine the writing of the query & the rule)
;; TODO: Clean up. Don't require passing botgh query & registry. Pull out as much reactive utility code as possible.
;; TODO: Ensure figwheel reloading still works, ensure rum-static still works

(defn- registry-key
  "Given a Rum state and a query, return the registry key it should use (that
   is, the query arguments)."
  [state query]
  (println "building registry key...")
  (let [num-inputs (count (:factui.api/inputs query))]
    ;; The first N Rum arguments, after the app state. Could improve this logic, maybe?
    (vec (take num-inputs (drop 1 (:rum/args state))))))

(defn- deregister
  "Deregister a Rum mixin from an active query"
  [state query registry]

  state
  )

(defn- listener
  "Given a Rum component's state, return a channel. When any value is put upon
   the channel, request a render of the given component."
  [state]
  (let [ch (a/chan (a/dropping-buffer 1))]

    (go-loop []
      (when (<! ch)
        (rum/request-render (:rum/react-component state))
        (recur)))

    ch))

(defn- register
  "Deregister a Rum mixin from an active query"
  [state query registry]
  (swap! registry update (registry-key state query)
    (fn [ch]
      (if ch ch (listener state))))
  (assoc state ::registered-args (:rum/args state)))

(defn- on-update
  "Called whenever the component is about to render, both the first time and
   subsequent times (:will-mount and :will-update)

   Takes the FactUI query and the Rum state and returns updated state."

  [state query registry]
  (if (= (:rum/args state) (::registered-args state))
    state
    (-> state
      (deregister query registry)
      (register query registry))))

(def ^:dynamic *results*)

;; Defines the following keys in the Rum state:
; ::results => atom containing latest query results (if any)
; ::registered-args => args registered to the query

(defn query
  "Given a FactUI query, construct a Rum mixin which will watch for changes to
   the given query, and re-render whenever the query results change.

   The following assumptions must hold, regarding the component:

   - The first argument to the component is an application state atom
   - The next N arguments are query inputs, where N is the number of inputs
   defined by the specified query.

   The component does not additional update semantics, but the FactUI mixin is
   fully composable with rum/static and rum/local."
  [query registry]
  {:will-mount #(on-update % query registry)

   :will-update #(on-update % query registry)

   :wrap-render (fn [render-fn]
                  (fn [state & render-args]
                    (let [[app-state & q-args] (:rum/args state)
                          results (apply f/query (:session @app-state) query q-args)]
                      (binding [*results* results]
                        (apply render-fn state render-args)))))

   :will-unmount (fn [state]
                   (deregister state query registry))})

(defn transact!
  "Swap a session-atom, updating it with the given txdata.

  Returns a map of tempid bindings."
  [app-state tx]
  (:bindings (swap! app-state
               (fn [state]
                 (let [session (:session state)
                       [new-session bindings] (f/transact session tx)]
                   (assoc state
                     :session new-session
                     :bindings bindings))))))

(def ^:private version (atom 0))

(defn app-state
  "Build and return an initial app-state atom, based on the provided FactUI
   session"
  [base]
  (let [app-state (atom {:session base
                         :version 0})]
    (add-watch version :version-watch
      (fn [_ _ _ version]
        (swap! app-state assoc :version version)))
    app-state))

(defn refresh
  "Invoke this function to force all components to re-render, globally.

  Useful for development, using Figwheel's :on-jsload"
  []
  (swap! version inc))