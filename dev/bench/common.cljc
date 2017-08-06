(ns bench.common)

;; Things to benchmark:

;; - Transaction time (for single datom, 10x datoms, many datoms)
;; - Render peformance (using React perftools), as a result of making transactions.


#?(:cljs (def samples (atom [])))


#?(:cljs (defn now [] (.getTime (js/Date.))))
