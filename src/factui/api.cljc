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
            [clojure.string :as str]
            #?(:clj [clojure.core.async :as a :refer [go go-loop <! >!]]
               :cljs [cljs.core.async :as a :refer [<! >!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

;; TODO: Allow schema to be defined *not* at compile time. We could do it,
;; would give a lot more operational flexibility.
#?(:clj (defmacro defsession
          "Define a new Datom session with the specified schema txdata, and
           the specified session ID."
          [name nses schema-txdata session-id]
          (let [base (gensym)]
            `(let [store# (store/store ~schema-txdata)]
               (cr/defsession ~base ~@nses
                 :fact-type-fn (store/fact-type-fn store#)
                 :ancestors-fn (store/ancestors-fn store#)
                 )
               (def ~name (session/session ~base store# ~session-id))))))

(defn with-id
  "Assign the session the given ID. Session IDs are used when registering
   reactive queries; the reaction will only trigger for sessions with the
   registered ID."
  [session id]
  (session/with-id session id))

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
   (defmacro defquery-static
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

;;TODO: Add an indentifier to sessions & registrations, to allow multiple concurrent registrations.
;; Currently, a registration will fire whenever a rule is fired from *anywhere*.
;; But sessions are immutable and forkable. So it makes sense to have a
;; "session-id" so that you can identify a single "logical" session across time
;; (or at least, one you want notifications on)


;; PROBLEM: Say a trigger sends 10 notifications. The registered listener grabs the first one, waits for the session, and notifies successfully. But then there is *another* notification queued up, with a session that will never be delivered.

;; Solution: close the session channel upon delivery and recur.

(defn register
  "Register a listener on a reactive query, for specific query inputs.
   Whenever the query triggers, with matching values, the results will be placed
   upon the returned results channel.

   The listener will be de-registered when the results channel is closed."
  [session-id q inputs]
  (let [registry (:factui/registry q)
        notify-ch (a/chan (a/sliding-buffer 1))
        results-ch (a/chan (a/sliding-buffer 1))
        registry-key [session-id (vec inputs)]]
    (swap! registry assoc registry-key notify-ch)
    (go-loop []
      (if-let [session (<! (<! notify-ch))]
        (let [results (apply query session q inputs)]
          (if (>! results-ch results)
            (recur)
            (swap! registry dissoc registry-key)))
        (recur)))
    results-ch))

(defn trigger
  "Function called as the right hand site of a reactive query."
  [registry input]
  (let [tx-complete-ch session/*tx-complete*
        session-id session/*session-id*]
    (when-let [notify-ch (get @registry [session-id input])]
      (a/put! notify-ch tx-complete-ch))))

#?(:clj
   (defmacro defquery
     "Define a reactive query. Arguments are the same as to `defquery`.

     The resulting query will have the following keys, in addition to those
     defined by `defquery`:

     :factui/registry - An atom containing a map of input vectors to
                        notification channels.
     :factui/rule - A Clara rule with the same conditions as the underlying
                    query query, which fires off a notification to
                    corresponding notifcation channels every time the query
                    changes."
     [& args]
     (let [input (s/conform ::fs/defquery-args args)
           output (comp/compile input comp/compile-defquery)
           inputs (vec (::cs/query-params output))
           input-symbols (mapv (comp symbol name) inputs)
           query-name (symbol (name (::cs/name output)))
           results-fn (gensym)
           registry-name (symbol (str *ns*)
                           (str (name (::cs/name output)) "__registry"))
           rule-name (symbol (str *ns*)
                       (str (name (::cs/name output)) "__rule"))]
       `(let [~results-fn ~(datalog-results input)]

          (defonce ~registry-name (atom {}))

          (cr/defrule ~rule-name
            ~@(s/unform ::cs/lhs (::cs/lhs output))
            ~'=>
            (trigger ~registry-name ~input-symbols))

          (cr/defquery ~@(s/unform ::cs/defquery-args output))

          ~(let [factui-data `{:factui/inputs ~inputs
                               :factui/result-fn ~results-fn
                               :factui/rule ~rule-name
                               :factui/registry ~registry-name}]
             (if (com/compiling-cljs?)
               `(set! ~query-name (merge ~query-name ~factui-data))
               `(alter-var-root (var ~query-name) merge ~factui-data)))))))