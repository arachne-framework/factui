(ns factui.test-runner
  (:require [factui.impl.session-test]
            [factui.api-test]
            [factui.api-reactive-test]
   #?(:clj [clojure.test :refer [deftest is are run-tests]]
      :cljs [cljs.test :refer-macros [deftest is are run-tests]])))

#?(:cljs (enable-console-print!))

#?(:cljs
   (defmethod cljs.test/report [:cljs.test/default :end-run-tests] [m]
     (if (cljs.test/successful? m)
       (.exit js/phantom 0)
       (.exit js/phantom 1))))

(defn -main
  "Entry point for running tests (until *.cljc tools catch up)"
  []
  (run-tests
    'factui.impl.session-test
    'factui.api-reactive-test
    'factui.api-test))

;; Run tests at the root level, in CLJS
#?(:cljs (-main))



