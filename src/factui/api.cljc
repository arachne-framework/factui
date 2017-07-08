(ns factui.api
  (:require [factui.session :as session]
            [factui.compiler :as comp]
            #?(:clj [clara.rules :as cr]
               :cljs [clara.rules :as cr :include-macros true])
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

#?(:clj
   (defmacro defsession
     "Wrapper for Clara's `defsession`. Arguments should resolve to:

      1. The name of the session,
      2. A collection of namespace names (from which Clara rules and queries
         will be loaded)
      3. A collection of Datomic-style schema entity maps."
     [name nses schema]
     (let [original-name (gensym)]
       `(do
          (cr/defsession ~original-name ~@nses)
          (def ~name (session/session ~original-name ~schema))))))

(defn now []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

(defn transact
  "Add Datomic-style transaction data to the session, returning a tuple of the
   new session and a map of the tempid bindings."
  [session txdata]
  (session/transact session txdata))

(defn transact-all
  "Apply multiple transactions sequentially, returning the updated session."
  [session & txes]
  (reduce (fn [s tx]
            (first (transact s tx)))
    session txes))

(defn transact!
  "Transact data in the consequence of a rule. Truth maintance will not be
   performed."
  [txdata]
  (session/transact! txdata false))

(defn transact-logical!
  "Transact data in the consequence of a rule. Truth maintenance will be
   perfomed (with associated caveats.)"
  [txdata]
  (session/transact! txdata true))

#_#?(:clj
   (defmacro defquery
     [name argvec query]
     `(cr/defquery ~name ~argvec
        ~@(comp/compile query))))

;; TODO: Test which of the following approaches is the most efficient:
;; 1. A Clara query, polled repeatedly
;; 2. A Clara rule that stores results (indexed by argument) in the fact store (or somewhere else)
;; 3. A Clara rule that pushes results to a channel (which can be filtered by argument).

