(ns factui.api
  (:require [factui.impl.session :as session]
            [factui.impl.store :as store]
            #?(:clj [factui.impl.compiler :as comp])
            #?(:clj [clara.rules :as cr]
               :cljs [clara.rules :as cr :include-macros true])
            #?(:clj [clara.rules.compiler :as com])
            [factui.specs :as fs]
            [factui.specs.clara :as cs]
            [factui.specs.datalog :as ds]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

#?(:clj (defmacro defsession
          "Define a new Datom session with the specified schema txdata"
          [name nses schema-txdata]
          (let [base (gensym)]
            `(let [store# (store/store ~schema-txdata)]
               (cr/defsession ~base ~@nses
                 :fact-type-fn (store/fact-type-fn store#)
                 :ancestors-fn (store/ancestors-fn store#)
                 )
               (def ~name (session/session ~base store#))))))

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

(defn- assert-variable-elem
  "Given a conformed find element (::ds/find-elem), throw an error if it is
   not a variable."
  [elem]
  (when-not (= ::ds/variable (first elem))
    (throw (ex-info "FactUI does not yet support pull expressions or aggregates in find clauses." {}))))

(defn- find-clause
  "Given conformed argumenst to defquery, return the find clause"
  [args]
  (or (-> args ::fs/query second ::ds/find)
      (-> args ::fs/query second :find)))

(defn- variables
  "Given conformed arguments to defquery, return a seq of Clara variable names"
  [args]
  (let [[_ elems] (find-clause args)]
    (vec (keep (fn [elem]
                 (when (sequential? elem)
                   (assert-variable-elem elem)
                   (keyword (second elem))))
           elems))))

(defmulti datalog-results
  "Given the conformed arguments to defquery, return syntax for a function
   which transforms its Clara-style query results to Datomic-style query
   results."
  (fn [args]
    (let [[type & _] (find-clause args)] type)))

(defmethod datalog-results ::ds/find-scalar
  [args]
  (let [[_ {elem ::ds/find-elem}] (find-clause args)]
    (assert-variable-elem elem)
    (let [v (keyword (second elem))]
      `(fn [r#] (get (first r#) ~v)))))

(defmethod datalog-results ::ds/find-tuple
  [args]
  (let [vs (variables args)]
    `(let [f# (apply juxt ~vs)]
       (fn [r#] (f# (first r#))))))

(defmethod datalog-results ::ds/find-coll
  [args]
  (let [vs (variables args)]
    `(let [f# ~(first vs)]
       (fn [r#] (set (map f# r#))))))

(defmethod datalog-results ::ds/find-rel
  [args]
  (let [vs (variables args)]
    `(let [f# (apply juxt ~vs)]
       (fn [r#] (set (map f# r#))))))


(s/fdef defquery :args ::fs/defquery-args)
#?(:clj
   (defmacro defquery
     "Define a Clara query using Datomic-style Datalog syntax."
     [& args]
     (let [input (s/conform ::fs/defquery-args args)
           ;_ (println "\n===========")
           ;_ (clojure.pprint/pprint input)
           output (comp/compile input comp/compile-defquery)
           ;_ (println "===========")
           ;_ (clojure.pprint/pprint output)
           ;_ (println "===========")
           clara (s/unform ::cs/defquery-args output)
           inputs (vec (::cs/query-params output))
           name (symbol (name (::cs/name output)))
           results-fn (gensym)]
       `(do
          (let [~results-fn ~(datalog-results input)]
            (cr/defquery ~@clara)
            ~(if (com/compiling-cljs?)
               `(set! ~name (assoc ~name ::inputs ~inputs
                                         ::result-fn ~results-fn))
               `(alter-var-root (var ~name) assoc ::inputs ~inputs
                                                  ::result-fn ~results-fn)))))))

(defn query-raw
  "Run a query that was defined using FactUI, using positional inputs.

  Return unprocessed (Clara-style) results."
  [session query & args]
  (let [inputs (::factui.api/inputs query)
        clara-args (interleave inputs args)]
    (apply cr/query session (:name query) clara-args)))

(defn results
  "Given Clara-style results from a given query return Datomic-style results.
   Does not re-run the query."
  [query results]
  (let [results-fn (::factui.api/result-fn query)]
    (when-not results-fn
      (throw (ex-info "Query did not specify a find clause - perhaps it was a basic Clara query, not one defined by FactUI?" {:query query})))
    (results-fn results)))

(defn query
  "Run a query that was defined using FactUI, using positional inputs.

   Returns Datomic-style results."
  [session query & args]
  (results query (apply query-raw session query args)))

(s/fdef defrule :args ::fs/defrule-args)

#?(:clj
   (defmacro defrule
     "Define a Clara rule. The left hand side may contain both Clara-style and
      Datalog-style conditions."
     [& args]
     (let [input (s/conform ::fs/defrule-args args)
           output (comp/compile input comp/compile-defrule)
           clara (s/unform ::cs/defrule-args output)]
       `(cr/defrule ~@clara))))