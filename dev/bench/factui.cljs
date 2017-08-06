(ns bench.factui
  (:require
   [factui.api :as f :include-macros true]
   [factui.rum :as fr :refer [*results*] :refer-macros [q]]
   [rum.core :as rum :include-macros true]))

(enable-console-print!)

(defn ^:export main
  []
  (println "hello world!")

  #_(let [app-state (fr/initialize
                    #'rulebase
                    schema
                    #'TaskList
                    (.getElementById js/document "root"))]
    (fr/transact! app-state initial-data)))