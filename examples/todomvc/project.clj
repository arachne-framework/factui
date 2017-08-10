(defproject todomvc-factui "1.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha17" :scope "provided"]
                 [org.clojure/clojurescript "1.9.671" :scope "provided"]
                 [rum "0.10.8"]
                 [org.arachne-framework/factui "1.1.0-SNAPSHOT"]]

  :plugins [[lein-cljsbuild "1.1.6" :exclusions [org.clojure/clojure]]
            [lein-figwheel "0.5.12" :exclusions [org.clojure/clojurescript]]]

  :resource-paths ["resources" "target"]

  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src"]
                        :figwheel {:on-jsload "factui.rum/refresh"}
                        :compiler {:main todomvc.factui
                                   :optimizations :none
                                   :asset-path "js/out"
                                   :output-to "target/public/js/app.js"
                                   :output-dir "target/public/js/out"
                                   :cache-analysis false}}
                       {:id "prod"
                        :source-paths ["src"]
                        :compiler {:main todomvc.factui
                                   :optimizations :advanced
                                   :output-to "target/public/js/app.js"
                                   :cache-analysis false}}]})
