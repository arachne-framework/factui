(ns todomvc.factui
  (:require [factui.api :as f :include-macros true]
            [factui.rum :as fr :refer [*results*] :refer-macros [q]]
            [rum.core :as rum :include-macros true]

            [todomvc.factui.ui :as ui]
            [todomvc.factui.schema :as schema]))

(enable-console-print!)

(f/rulebase rulebase todomvc.factui todomvc.factui.ui)

(def initial-data
  [{:db/ident :global
    :global/view-mode :all
    :global/new-task {:task/name ""}}])

(defn ^:export main
  []
  (let [mount #(rum/mount (@#'ui/App %) (.getElementById js/document "root"))
        app-state (fr/initialize #'rulebase schema/schema mount)]
    (fr/transact! app-state initial-data)))
