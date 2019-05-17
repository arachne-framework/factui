(defproject org.arachne-framework/factui "1.1.0-SNAPSHOT"

  ;; tools.deps
  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}

  :profiles {:test {:lein-tools-deps/config {:aliases [:clj :test]}
                    :plugins [[lein-shell "0.4.0" :exclusions [org.clojure/clojure]]
                              [lein-cljsbuild "1.1.6" :exclusions [org.clojure/clojure]]]
                    :clean-targets ^{:protect false} ["target" :target-path]
                    :cljsbuild {:builds [{:id "dev-figwheel"
                                          :source-paths ["src" "dev"]
                                          :figwheel {:on-jsload "factui.rum/refresh"}
                                          :compiler {:main factui.ui.dev
                                                     :optimizations :none
                                                     :asset-path "js/out"
                                                     :output-to "target/public/js/dev.js"
                                                     :output-dir "target/public/js/out"
                                                     :cache-analysis false}}
                                         {:id "dev"
                                          :source-paths ["src" "dev"]
                                          :compiler {:main factui.ui.dev
                                                     :optimizations :advanced
                                                     ;:pretty-print true
                                                     ;:source-map "target/public/js/dev.map.js"
                                                     :asset-path "js/out"
                                                     :output-to "target/public/js/dev.js"
                                                     :output-dir "target/public/js/dev"
                                                     :cache-analysis false}}
                                         {:id "test-whitespace"
                                          :source-paths ["src" "dev" "test"]
                                          :compiler {:main factui.test-runner
                                                     :optimizations :whitespace
                                                     :output-to "target/test-ws.js"
                                                     :output-dir "target/test-ws"
                                                     :cache-analysis false}}
                                         {:id "test-advanced"
                                          :source-paths ["src" "dev" "test"]
                                          :compiler {:main factui.test-runner
                                                     :externs ["resources/phantomjs-externs.js"]
                                                     :optimizations :advanced
                                                     :output-to "target/test-adv.js"
                                                     :output-dir "target/test-adv"
                                                     :cache-analysis false}}
                                         {:id "test-bench"
                                          :source-paths ["src" "dev" "test"]
                                          :compiler {:main factui.bench
                                                     :externs ["resources/phantomjs-externs.js"]
                                                     :optimizations :advanced
                                                     :output-to "target/test-bench.js"
                                                     :output-dir "target/test-bench"
                                                     :cache-analysis false}}]}
                    :aliases {"test-clj" ["run" "-m" "factui.test-runner"]
                              "test-cljs" ["do" ["clean"]
                                           ["cljsbuild" "once" "test-advanced"]
                                           ["shell" "phantomjs" "target/test-adv.js"]]
                              "test-all" ["do" ["test-clj"]
                                          ["test-cljs"]]
                              "bench" ["do" ["clean"]
                                       ["cljsbuild" "once" "test-bench"]
                                       ["shell" "phantomjs" "target/test-bench.js"]]
                              }}
             :dev {:lein-tools-deps/config {:aliases [:clj :dev]}
                   :plugins [[lein-figwheel "0.5.12" :exclusions [org.clojure/clojurescript]]]
                   :clean-targets ^{:protect false} ["target" :target-path]
                   :figwheel {:open-file-command "emacsclient"}}})
