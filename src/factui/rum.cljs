(ns factui.rum
  (:require [rum.core :as rum]
            [factui.api :as f :include-macros true]
            [cljs.core.async :as a]
            [factui.impl.store :as store])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(def ^:private rebuild-on-refresh (atom true))

(defn set-rebuild-on-refresh
  "Sets whether the session be should be entirely rebuilt on a new rulebase,
   when a refresh manually triggered (such as by a Figwheel recompile).
   Defaults to true.

   If set to false, changes to rules will not be reflected without a full page
   refresh.

   However, this operation may be expensive with very large sessions. If
   refreshing takes too long, you may wish to disable this."
  [rebuild?]
  (reset! rebuild-on-refresh rebuild?))

(defn- query-args
  "Given a Rum state and a query, return a vectory of FactUI query args, based
   on the Rum arguments."
  [state query]
  (let [num-inputs (count (:factui/inputs query))]
    ;; The first N Rum arguments, after the app state. Could improve this logic, maybe?
    (vec (take num-inputs (drop 1 (:rum/args state))))))

(defn- deregister
  "Deregister a Rum mixin from an active query"
  [state query args]
  (when-let [ch (::results-ch state)]
    (swap! (::session-atom state) f/deregister query args ch)
    (a/close! ch))
  (dissoc state ::query-args ::results-ch ::session-atom))

;; Defines the following keys in the Rum state:
; ::results => atom containing latest query results (if any)
; ::results-ch => channel upon which will be placed new results
; ::query-args => args registered to the query
; ::session-atom => atom containing the session to which the query was registered.

(defn- register
  "Register a Rum component"
  [state query session args]
  (let [results (atom (apply f/query @session query args))
        ch (a/chan)]
    (swap! session f/register query args ch)
    (go-loop []
      (when-let [r (a/<! ch)]
        (reset! results r)
        (rum/request-render (:rum/react-component state))
        (recur)))
    (assoc state ::query-args args
                 ::session-atom session
                 ::results results
                 ::results-ch ch)))

(defn- on-update
  "Called whenever the component is about to render, both the first time and
   subsequent times (:will-mount and :will-update)

   Takes the FactUI query and the Rum state and returns updated state.
   Deregisters and re-registers the query with new arguments whenever the query
   arguments change."
  [state query]
  (let [session (first (:rum/args state))
        args (query-args state query)]
    (if (and (= session (::session-atom state))
             (= args (::query-args state)))
      state
      (-> state
        (deregister query args)
        (register query session args)))))

(def ^:dynamic *results*)

(defn mixin
  "Construct a Rum mixin based on the given query, which must be a reactive
   FactUI query (such as defined by `factui.api/defquery`.)

  Components using this mixin expect the following:

   - The first argument to the component constructor must be an application state atom
   - The next N arguments are passed to the query as inputs, where N = the
     number of inputs defined by the specified query.

  Inside the component's render body, the `factui.rum/*results*` dynamic var
  will be bound to the query results.

  This mixin will cause the Rum component to be re-rendered whenever the query
  results change. It does not affect Rum's component update semantics in any
  other way, and may be freely combined with other Rum mixins (`rum/static` is
  reccomended.)"
  [query]

  {:will-mount #(on-update % query)

   :will-update #(on-update % query)

   :wrap-render (fn [render-fn]
                  (fn [state & render-args]
                    (binding [*results* @(::results state)]
                      (apply render-fn state render-args))))

   :will-unmount (fn [state]
                   (deregister state query (::query-args state)))})

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
  "Initialize an application with the specified rulebase and schema, calling
   the provided callback once when initialization is complete, and again
   whenever the page is reloaded during development (e.g, due to a Figwheel
   reload.)

   The provided callback is passed an atom containing a session.

   Note that the rulebase must be provided as a Var, not as a value, to ensure
   that new values can be retrieved after a Figwheel reload."
  [rulebase schema on-load]
  (let [app-state-holder (atom (atom (f/session @rulebase schema)))]

    (on-load @app-state-holder)

    (add-watch version :version-watch
      (fn [_ _ _ version]
        (let [old-session @@app-state-holder
              new-session (if @rebuild-on-refresh
                            (f/rebuild-session @rulebase old-session schema)
                            old-session)]
          (reset! app-state-holder (atom new-session))
          (on-load @app-state-holder))))

    @app-state-holder))

(defn refresh
  "Invoke this function to force all components to re-render, globally.

  Useful for development, using Figwheel's :on-jsload"
  []
  (swap! version inc))

(defn datoms
  "Return a lazy seq of all the datoms in the given FactUI session. Not
   particularly efficient, useful for debugging purposes."
  [session]
  (store/datoms (:store session)))

(defn then
  "Convenience function. Return a simple event handler function that transacts
   the given data. Accepts plain entity maps as well as normal txdata. Swallows
   the event, preventing it from propagating."
  [app-state txdata]
  (let [txdata (if (map? txdata) [txdata])]
    (fn [evt]
      (.preventDefault evt)
      (transact! app-state txdata))))
