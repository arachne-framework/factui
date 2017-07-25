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

;; TODO: prove concept DONE
;; TODO: Write "reactive-query" macro (to combine the writing of the query & the rule)
;; TODO: Clean up. Don't require passing botgh query & registry. Pull out as much reactive utility code as possible.
;; TODO: Ensure figwheel reloading still works, ensure rum-static still works

(defn- query-args
  "Given a Rum state and a query, return a vectory of FactUI query args, based
   on the Rum arguments."
  [state query]
  (let [num-inputs (count (:factui/inputs query))]
    ;; The first N Rum arguments, after the app state. Could improve this logic, maybe?
    (vec (take num-inputs (drop 1 (:rum/args state))))))

(defn- deregister
  "Deregister a Rum mixin from an active query"
  [state query session-id]
  (when (::results-ch state)
    (a/close! (::results-ch state)))
  (dissoc state ::registered-args ::results-ch))

(defn- register
  "Deregister a Rum mixin from an active query"
  [state query session-id]
  (let [session (first (:rum/args state))
        args (query-args state query)
        results (atom (apply f/query (:session @session) query args))
        ch (f/register session-id query args)]
    (go-loop []
      (when-let [r (a/<! ch)]
        (reset! results r)
        (rum/request-render (:rum/react-component state))
        (recur)))

    (assoc state ::registered-args (:rum/args state)
                 ::results results
                 ::results-ch ch)))

(defn- on-update
  "Called whenever the component is about to render, both the first time and
   subsequent times (:will-mount and :will-update)

   Takes the FactUI query and the Rum state and returns updated state."

  [state query session-id]
  (if (= (:rum/args state) (::registered-args state))
    state
    (-> state
      (deregister query session-id)
      (register query session-id))))

(def ^:dynamic *results*)

;; Defines the following keys in the Rum state:
; ::results => atom containing latest query results (if any)
; ::results-ch => channel upon which will be placed new results
; ::registered-args => args registered to the query

(defn query
  "Given a FactUI reactive query, construct a Rum mixin which will watch for
   changes to the given query, and re-render whenever the query results change.

   The following assumptions must hold, regarding the component:

   - The first argument to the component is an application state atom
   - The next N arguments are query inputs, where N is the number of inputs
   defined by the specified query.

   The component does not additional update semantics, but the FactUI mixin is
   fully composable with rum/static and rum/local."
  [query session-id]
  {:will-mount #(on-update % query session-id)

   :will-update #(on-update % query session-id)

   :wrap-render (fn [render-fn]
                  (fn [state & render-args]
                    (binding [*results* @(::results state)]
                      (apply render-fn state render-args))))

   :will-unmount (fn [state]
                   (deregister state query session-id))})

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