(ns factui.rum
  (:require [rum.core :as rum]
            [factui.api :as f :include-macros true]
            [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(def ^:private *rebuild-on-refresh* (atom true))

(defn set-rebuild-on-refresh
  "Sets whether the session be should be entirely rebuilt on a new rulebase,
   when a refresh manually triggered (such as by a Figwheel recompile).
   Defaults to true.

   If set to false, changes to rules will not be reflected without a full page
   refresh.

   However, this operation may be expensive with very large sessions. If
   refreshing takes too long, you may wish to disable this."
  [rebuild?]
  (reset! *rebuild-on-refresh* rebuild?))

(defn- query-args
  "Given a Rum state and a query, return a vectory of FactUI query args, based
   on the Rum arguments."
  [state query]
  (let [num-inputs (count (:factui/inputs query))]
    ;; The first N Rum arguments, after the app state. Could improve this logic, maybe?
    (vec (take num-inputs (drop 1 (:rum/args state))))))

(defn- deregister
  "Deregister a Rum mixin from an active query"
  [state query]
  (when (::results-ch state)
    (a/close! (::results-ch state)))
  (dissoc state ::registered-args ::results-ch))

(defn- register
  "Deregister a Rum mixin from an active query"
  [state query]
  (let [session (first (:rum/args state))
        session-id (:session-id @session)
        args (query-args state query)
        results (atom (apply f/query @session query args))
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

  [state query]
  (if (= (:rum/args state) (::registered-args state))
    state
    (-> state
      (deregister query)
      (register query))))

(def ^:dynamic *results*)

;; Defines the following keys in the Rum state:
; ::results => atom containing latest query results (if any)
; ::results-ch => channel upon which will be placed new results
; ::registered-args => args registered to the query

(defn query2
  "Given a FactUI reactive query, construct a Rum mixin which will watch for
   changes to the given query, and re-render whenever the query results change.

   The following assumptions must hold, regarding the component:

   - The first argument to the component is an application state atom
   - The next N arguments are query inputs, where N is the number of inputs
   defined by the specified query.

   The component does not additional update semantics, but the FactUI mixin is
   fully composable with rum/static and rum/local."
  [query]
  {:will-mount #(on-update % query)

   :will-update #(on-update % query)

   :wrap-render (fn [render-fn]
                  (fn [state & render-args]
                    (binding [*results* @(::results state)]
                      (apply render-fn state render-args))))

   :will-unmount (fn [state]
                   (deregister state query))})

;; Used to convey bindings out of a "transact" swap.
(def ^:dynamic *bindings*)

(defn transact!
  "Swap a session-atom, updating it with the given txdata.

  Returns a map of tempid bindings."
  [app-state tx]
  (binding [*bindings* nil]
    (swap! app-state
      (fn [session]
        (let [[new-session bindings] (f/transact session tx)]
          (set! *bindings* bindings)
          new-session)))
    *bindings*))

(defonce ^:private version (atom 0))

(defn initialize
  "Start application rendering to the given root node, given a base Clara session"
  [rulebase schema root-component root-element]
  (let [app-state-holder (atom (atom (f/session @rulebase schema)))
        render #(rum/mount (root-component @app-state-holder) root-element)]

    (render)

    (add-watch version :version-watch
      (fn [_ _ _ version]
        (let [old-session @@app-state-holder
              new-session (if @*rebuild-on-refresh*
                            (f/rebuild-session @rulebase old-session schema)
                            old-session)]
          (reset! app-state-holder (atom new-session))
          (render))))

    @app-state-holder))

(defn refresh
  "Invoke this function to force all components to re-render, globally.

  Useful for development, using Figwheel's :on-jsload"
  []
  (swap! version inc))