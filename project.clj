(defproject factui "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/core.async "0.3.443"]
                 [com.cerner/clara-rules "0.15.0" :exclusions [prismatic/schema]]
                 [prismatic/schema "1.1.6"]]
  :profiles {:dev {:plugins [[lein-cljsbuild "1.1.6" :exclusions [org.clojure/clojure]]
                             [lein-figwheel "0.5.10" :exclusions [org.clojure/clojurescript]]]
                   :dependencies [[org.clojure/clojurescript "1.9.562" :exclusions [org.clojure/tools.reader]]
                                  [rum "0.10.8"]
                                  [figwheel-sidecar "0.5.10"]]
                   :source-paths ["dev"]
                   :resource-paths ["target"]
                   :clean-targets ^{:protect false} ["target" :target-path]
                   :cljsbuild {:builds [{:id "factui-dev"
                                         :source-paths ["src" "dev"]
                                         :figwheel true
                                         :compiler {:main factui.ui.bench
                                                    :asset-path "js/out"
                                                    :output-to "target/public/js/demo.js"
                                                    :output-dir "target/public/js/out"
                                                    :cache-analysis false
                                                    :source-map-timestamp true}}]}
                   :figwheel {:open-file-command "emacsclient"}}})
