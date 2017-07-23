(ns factui.ui.dev
  (:require [factui.api :as f :include-macros true]
            [factui.rum :as fr :refer [*results*]]
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

(f/defquery task-q
  [:find [?title ?completed]
   :in ?task
   :where
   [?task :task/title ?title]
   [?task :task/completed ?completed]])

(rum/defc Task < {:key-fn (fn [_ id] id)}
                 (fr/query task-q)
  [app-state ?task]
  (let [[title completed] *results*]
    [:li (str (if completed
                "DONE: "
                "TODO: ") title)]))

(f/defquery tasklist-q
  [:find [?t ...]
   :where
   [?t :task/title _]])

(rum/defc TaskList < (fr/query tasklist-q)
                     rum/static
  [app-state title]
  [:div
     [:h1 title]
     [:ul (for [t *results*]
            (Task app-state t))]
   [:button {:on-click (fn []
                         (js/alert "Adding task!"))}
    "Add Task"]])

(f/defsession base ['factui.ui.dev] schema)

(def initial-data
  [{:task/title "Task A"
    :task/completed false}
   {:task/title "Task B"
    :task/completed false}
   {:task/title "Task C"
    :task/completed false}])

(defn ^:export main
  []
  (let [app-state (fr/app-state base)
        root (.getElementById js/document "root")]

    (rum/mount (TaskList app-state "Tasks") root)

    (fr/transact! app-state initial-data)

    (js/setTimeout (fn []
                     (fr/transact! app-state [{:db/id 10001
                                               :task/completed true}]))
      4000)))