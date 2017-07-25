(ns factui.rum
  (:require [rum.core :as rum]
            [factui.api :as f :include-macros true]
            [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

;; TODO: Add pulse predicates, create demo of event handling
;; TODO: See if it's worth moving rules engine to web worker

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

(defonce ^:private version (atom 0))


(defn initialize
  "Start application rendering to the given root node, given a base Clara session"
  [base-session root-component root-element]
  (let [app-state (atom {:session base-session})]

    (rum/mount (root-component app-state) root-element)

    (add-watch version :version-watch
      (fn [_ _ _ version]

        (rum/mount (root-component (atom {:session (:session @app-state)})) root-element)

        ))
    app-state))

(defn refresh
  "Invoke this function to force all components to re-render, globally.

  Useful for development, using Figwheel's :on-jsload"
  []
  (swap! version inc))