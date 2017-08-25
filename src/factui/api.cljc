(ns factui.api
  (:require [factui.facts :as f]
            [factui.impl.session :as session]
            [factui.impl.store :as store]
            [factui.impl.rules]
            [factui.specs :as fs]
            [factui.specs.clara :as cs]
            [factui.specs.datalog :as ds]
            [clojure.walk :as w]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
   #?(:clj  [factui.impl.compiler :as comp])
   #?(:clj  [clara.rules :as cr]
      :cljs [clara.rules :as cr :include-macros true])
   #?(:clj  [clara.rules.compiler :as com])
   #?(:clj  [clojure.core.async :as a :refer [go go-loop <! >!]]
      :cljs [cljs.core.async :as a :refer [<! >!]])
   #?(:clj  [clojure.pprint :as pprint]
      :cljs [cljs.pprint :as pprint]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

#?(:clj (defmacro rulebase
          "Define a new immutable base session with the specified name, and
           rules/queries loaded from the specified namespaces (as in Clara).
           Rules are assembled and compiled at compile time.

          This session should be converted to a FactUI session and a schema
          installed before adding any data (see `factui.api/session`)."
          [name & nses]
          (let [nses (conj nses 'factui.impl.rules)]
            `(cr/defsession ~name ~@(map (fn [n] `(quote ~n)) nses)
               :fact-type-fn f/fact-type-fn
               :ancestors-fn f/ancestors-fn))))

(defn session
  "Given a base session (as defined by `factui.api/rule-base`), add in a
   schema and return an initalized FactUI session."
  [base schema-txdata]
  (session/session base (store/store schema-txdata)))

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
     "Define a basic Clara query using Datomic-style Datalog syntax.

     The resulting query will have the following additional keys defined:

     :factui/inputs - Vector of symbols used for the query's `:in` clause.
     :factui/result-fn - A function that can be called on the return value
                         from `clara.rules/query` to obtain Datomic-style results."
     [& args]
     (let [input (s/conform ::fs/defquery-args args)
           output (comp/compile input comp/compile-defquery)
           clara (s/unform ::cs/defquery-args output)
           inputs (vec (::cs/query-params output))
           name (symbol (name (::cs/name output)))
           results-fn (gensym)]
       `(do
          (let [~results-fn ~(datalog-results input)]
            (cr/defquery ~@clara)
            ~(if (com/compiling-cljs?)
               `(set! ~name (assoc ~name :factui/inputs ~inputs
                                         :factui/result-fn ~results-fn))
               `(alter-var-root (var ~name) assoc :factui/inputs ~inputs
                                                  :factui/result-fn ~results-fn)))))))

(defn query-raw
  "Run a query that was defined using FactUI, using positional inputs.

  Return unprocessed (Clara-style) results."
  [session query & args]
  (let [inputs (:factui/inputs query)
        clara-args (interleave inputs args)]
    (apply cr/query session (:name query) clara-args)))

(defn results
  "Given Clara-style results from a given query return Datomic-style results.
   Does not re-run the query."
  [query results]
  (let [results-fn (:factui/result-fn query)]
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

(defn rebuild-session
  "Rebuild a session on a new base ruleset by re-adding all datoms"
  [base old-session schema]
  #?(:clj (println "Rebuilding session due to code reload. Logical rule state will be lost. Rebuilding...")
     :cljs (.log js/console "Rebuilding session due to code reload. Logical rule state will be lost. Rebuilding..."))
  (let [new-session (session base schema)
        datoms (store/datoms (:store old-session))]
    (when-not (empty? datoms)
      (let [ops (map (fn [[e a v]] [:db/add e a v]) datoms)
            populated-session (transact-all new-session ops)]
        #?(:clj  (println "...rebuild complete.")
           :cljs (.log js/console "...rebuild complete."))
        populated-session))))

(defn register
  "Register a channel which will recieve notification when a query's results change."
  [session query args ch]
  (let [results-ch (a/chan (a/sliding-buffer 1))]
    (go
      (loop []
        (let [raw-results (a/<! results-ch)]
          (when raw-results
            (a/>! ch ((:factui/result-fn query) raw-results))
            (recur)))))
    (let [params (zipmap (:factui/inputs query) args)]
      (session/register session
        (:name query)
        params
        results-ch))))

(defn deregister
  "Remove the registration of a channel to recieve notification when a query's results change."
  [session query args ch]
  (let [params (zipmap (:factui/inputs query) args)]
    (session/deregister session
      (:name query)
      params
      ch)))

(defn clean-tx
  "Convenience function to remove nil values from txdata.

  Applicaiton code used to generate txdata can be much cleaner, if it is
  allowed to emit nil values."
  [txdata]
  (w/prewalk (fn [n]
               (cond
                 (map? n) (into {} (filter (fn [[_ v]] (not (nil? v))) n))
                 (coll? n) (filterv identity n)
                 :else n))
    txdata))