(defproject factui "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [org.clojure/core.async "0.3.443"]
                 [com.cerner/clara-rules "0.15.0" :exclusions [prismatic/schema]]
                 [prismatic/schema "1.1.6"]]
  :source-paths ["dev" "src"]
  :profiles {:test {:plugins [[lein-shell "0.4.0" :exclusions [org.clojure/clojure]]
                              [lein-cljsbuild "1.1.6" :exclusions [org.clojure/clojure]]]
                    :dependencies [[org.clojure/clojurescript "1.9.562"
                                   :exclusions [org.clojure/tools.reader]]]
                    :cljsbuild {:builds [{:id "factui-dev"
                                          :source-paths ["src" "dev"]
                                          :figwheel true
                                          :compiler {:main factui.ui.bench
                                                     :asset-path "js/out"
                                                     :output-to "target/public/js/demo.js"
                                                     :output-dir "target/public/js/out"
                                                     :cache-analysis false
                                                     :source-map-timestamp true}}
                                         {:id "test-whitespace"
                                          :source-paths ["src" "dev" "test"]
                                          :compiler {:main factui.test-runner
                                                     :output-to "target/test-ws.js"
                                                     :cache-analysis false}}
                                         {:id "test-advanced"
                                          :source-paths ["src" "dev" "test"]
                                          :compiler {:main factui.test-runner
                                                     :externs ["resources/phantomjs-externs.js"]
                                                     :output-to "target/test-adv.js"
                                                     :cache-analysis false}}]}
                    :aliases {"test-clj" ["run" "-m" "factui.test-runner"]
                              "test-cljs" ["do" ["clean"]
                                           ["cljsbuild" "once" "test-whitespace"]
                                           ["shell" "phantomjs" "target/test-ws.js"]]
                              "test-all" ["do" ["test-clj"]
                                               ["test-cljs"]]}}
             :dev {:plugins [[lein-figwheel "0.5.10" :exclusions [org.clojure/clojurescript]]]
                   :dependencies [[rum "0.10.8"]
                                  [figwheel-sidecar "0.5.10"]]
                   :source-paths ["dev" "test"]
                   :resource-paths ["target"]
                   :clean-targets ^{:protect false} ["target" :target-path]

                   :figwheel {:open-file-command "emacsclient"}}})
