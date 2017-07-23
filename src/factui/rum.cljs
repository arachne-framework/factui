(ns factui.rum
  (:require [rum.core :as rum]
            [factui.api :as f :include-macros true]))

;; DONE: Allow non-query args (& validate that they correctly force an update)
;; DONE: Test figwheel forced updates
;; MAYBE: Allow passing in a different app-state atom on the fly...? Probably not worth it.
;; TODO: Add pulse predicates, create demo of events
;; TODO: Create wrapper macro to make cleaner

(defn- start-watching
  "Given a Rum state and a query, begin watching the current app-state atom
   for changes to the specified query, using the current arguments.

   Returns an updated Rum state."
  [state query]
  (let [watch-key (gensym "factui-watcher")
        [app-state & args] (:rum/args state)
        session (:session @app-state)
        results (atom (apply f/query-raw session query args))
        component (:rum/react-component state)]

    (add-watch app-state watch-key
      (fn [_ _ old-app-state new-app-state]
        (if (not= (:version old-app-state)
                  (:version new-app-state))
          (rum/request-render component)
          (when (not (== (:session old-app-state)
                         (:session new-app-state)))
            (let [old-results @results
                  new-results (apply f/query-raw
                                (:session new-app-state)
                                query args)]
              (when (not= old-results new-results)
                (reset! results new-results)
                (rum/request-render component)))))))

    (assoc state ::results results
                 ::watch-key watch-key
                 ::watched-args (:rum/args state))))

(defn- stop-watching
  "Stop watching the app-state atom"
  [state]
  (let [[app-state & _] (::watched-args state)]
    (when app-state
      (remove-watch app-state (::watch-key state))))
  state)

(defn- on-update
  "Called whenever the component is about to render, both the first time and
   subsequent times (:will-mount and :will-update.)

   Takes the FactUI query and the Rum state and returns updated state."

  [query state]
  (if (= (:rum/args state) (::watched-args state))
    state
    (-> state
      (stop-watching)
      (start-watching query))))

(def ^:dynamic *results*)

;; Defines the following keys in the Rum state:
; ::results => atom containing latest query results (if any)
; ::watch-key => key being used to watch the current args
; ::watched-args => the atom & args used for the current watcher

(defn query
  "Given a FactUI query, construct a Rum mixin which will watch for changes to
   the given query, and re-render whenever the query results change.

   The following assumptions must hold, regarding the component:

   - The first argument to the component is an application state atom
   - The next N arguments are query inputs, where N is the number of inputs
   defined by the specified query.

   The component does not additional update semantics, but the FactUI mixin is
   fully composable with rum/static and rum/local."
  [query]
  {:will-mount #(on-update query %)

   :will-update #(on-update query %)

   :wrap-render (fn [render-fn]
                  (fn [state & args]
                    (let [results (f/results query @(::results state))]
                      (binding [*results* results]
                        (apply render-fn state args)))))

   :will-unmount (fn [state]
                   ;:TODO :deregister listener
                   state
                   )

   :should-update (fn [old-state new-state]
                    true)})

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