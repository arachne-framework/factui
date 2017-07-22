(ns factui.ui.dev
  (:require [factui.api :as f :include-macros true]
            [clara.rules :as cr :include-macros true]
            [rum.core :as rum :include-macros true]))

(enable-console-print!)

(def schema
  [{:db/ident :task/title
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :task/completed
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one}])

(defn the-mixin
  [query]
  {:will-mount
   (fn [state]
     (let [watch-key (gensym "factui-watcher")
           [app-state & args] (:rum/args state)
           session (:session @app-state)
           initial-results (apply f/query-raw session query args)
           results (atom initial-results)
           component (:rum/react-component state)]

       (add-watch app-state watch-key
         (fn [_ _ old-app-state new-app-state]
           (println "watcher triggered")
           (if (not= (:version old-app-state) (:version new-app-state))
             (rum/request-render component)
             (when (not (== (:session old-app-state)
                            (:session new-app-state)))
               (let [old-results @results
                     new-results (apply f/query-raw
                                   (:session new-app-state)
                                   query args)]
                 (when (not= old-results new-results)
                   (println "got new results:" new-results)
                   (reset! results new-results)
                   (rum/request-render component)))))))

       (assoc state ::raw-results results
                    ::watch-key watch-key
                    ::results (f/results query initial-results))))

   :will-update
   (fn [state]
     (assoc state ::results
                  (f/results query @(::raw-results state))))

   :will-unmount
   (fn [state]
     (let [[app-state & _] (:rum/args state)]
       (remove-watch app-state watcher-key)))

   :should-update
   (fn [old-state new-state]
     true)})

(f/defquery task-q
  [:find [?title ?completed]
   :in ?t
   :where
   [?t :task/title ?title]
   [?t :task/completed ?completed]])

(rum/defcs Task < {:key-fn (fn [_ id] id)}
                  (the-mixin task-q)
  [state app-state task]
  (println "rendering Task:" (::results state))
  (let [[title completed] (::results state)]
    [:li (str (if completed
                "DONE: "
                "TODO: ") title)]))

(f/defquery tasklist-q
  [:find [?t ...]
   :where
   [?t :task/title _]])

(rum/defcs TaskList < (the-mixin tasklist-q)
  [state app-state]
  (println "Rendering TaskList")
  (let [results (::results state)]
    [:div
     [:h1 "Tasks"]
     [:ul (for [t results]
            (Task app-state t))]
     [:button {:on-click (fn []
                           (js/alert "Adding task!"))}
      "Add Task"]]))

(f/defsession base ['factui.ui.dev] schema)

(def initial-data
  [{:task/title "Task A"
    :task/completed false}
   {:task/title "Task B"
    :task/completed false}
   {:task/title "Task C"
    :task/completed false}])

(defn transact!
  "Wrapper for f/transact that swaps the rule session in an app state.

  Returns a map of tempid bindings."
  [app-state tx]
  (:bindings (swap! app-state
               (fn [state]
                 (let [session (:session state)
                       [new-session bindings] (f/transact session tx)]
                   (assoc state
                     :session new-session
                     :bindings bindings))))))

(defn ^:export main
  []
  (let [app-state (atom {:session base
                         :version 0})
        root (.getElementById js/document "root")]

    (rum/mount (TaskList app-state) root)

    (transact! app-state initial-data)

    (js/setTimeout (fn []
                     (println "updating data...")
                     (transact! app-state [{:db/id 10001
                                            :task/completed true}]))
      3000))

  )