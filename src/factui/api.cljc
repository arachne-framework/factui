(ns factui.api
  (:require [factui.session :as session]
            [factui.store :as store]
            #?(:clj [factui.compiler :as comp])
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

(s/fdef defquery :args ::fs/defquery-args)

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

(defn query
  "Run a query that was defined using FactUI, returing Datomic-style results"
  [session query & args]
  (let [inputs (::factui.api/inputs query)
        clara-args (interleave inputs args)
        results-fn (::factui.api/result-fn query)
        results (apply cr/query session (:name query) clara-args)]
    (when-not results-fn
      (throw (ex-info "Query did not specify a find clause - perhaps it was a basic Clara query, not one defined by FactUI" {})))
    (results-fn results)))

(comment

  (defquery all-attrs-clara
    "Find all attributes of an entity, using a mixed Clara clause"
    [:find ?attr ?value
     :in ?id
     :where
     [?eid :person/id ?id]
     [factui.facts.Datom (= e ?eid) (= a ?attr) (= v ?value)]])

  (clojure.pprint/pprint
    (macroexpand-1
      '(defquery person-name-map
        "Find a person by their name"
        {:find [?name]
         :in [?id]
         :where [[?id2 :person/name ?name2]
                 [?id1 :person/name ?name1]]})))

  {:factui.specs/name person-name-map,
   :factui.specs/docstr "Find a person by their name",
   :factui.specs/query
   [:factui.specs/map-query
    {:find
     [:factui.specs.datalog/find-rel
      [[:factui.specs.datalog/variable ?name]]],
     :in
     [[:factui.specs.datalog/binding
       [:factui.specs.datalog/bind-scalar ?id]]],
     :where
     [:factui.specs/clara-condition
      [:factui.specs.clara/fact-constraint
       {:factui.specs.clara/fact-type [?id :person/name ?name]}]]}]}

  (defquery person-name-map
    "Find a person by their name"
    {:find [?name]
     :in [?id]
     :where [[?id :person/name ?name]]})

  (defquery person-name
    "Find a person by their name"
    [:find ?name
     :in ?id
     :where [?id :person/name ?name]])

  )


;; TODO: Test which of the following approaches is the most efficient:
;; 1. A Clara query, polled repeatedly
;; 2. A Clara rule that stores results (indexed by argument) in the fact store (or somewhere else)
;; 3. A Clara rule that pushes results to a channel (which can be filtered by argument).



(comment
  ;; WHAT THE APIS SHOULD LOOK LIKE
  ;; Let's see how far we can get without aggregates or pull expressions

  ;; STYLE #1: Heavily Datomic-inspired

  ;; Simple query
  (defquery person-age
    [:find ?age
     :in ?id
     :where
     [?p :person/id ?id]
     [?p :person/age ?age]])

  ;; Query with aggregating result
  (defquery person-children
    [:find ?age (distinct ?children)
     :in ?id
     :where
     [?p :person/id ?id]
     [?p :person/age ?age]
     [?p :person/children ?children]])

  ;; Query invocation...
  (query session person-children 42)

  ;; Maybe? Auto-binding in body is kind of cool tho...
  (defrule person-friends
    [:find ?p1 ?p2
     :where
     [?p1 :person/friends ?p2]
     (not [?p2 :person/friends ?p1])]
    (fn [p1 p2]
      (transact! [{:db/id p1
                   :person/friends p2}])))

  ;; Auto-binding in the body since we can't do any kind of grouping or roll-up for a rule, anyway...
  (defrule person-friends
    [?p1 :person/friends ?p2]
    (not [?p2 :person/friends ?p1])
    =>
    (transact! [{:db/id ?p1
                 :person/friends ?p2}]))

  ;; Problem. a rule can't contain any kind of :find or :with, since it can't aggregate results. Each result tuple results in a separate invocation of the body.

  ;; But what about when you *want* aggregates? Got to do those in the clauses I guess. Which implies a grammar different from Datomic.

  ;; But if that's the rule for rules, should we have different rules for queries? I don't think so.

  ;; Maybe it's ok having a more clara-like syntax for rules, and a more Datomic-like syntax for components

  ;; Note that we might need to add the ability to pass custom Props here. Not sure if we'll need it.
  (defcomponent PersonCard

    [:find ?name ?age ?child ?child-order
     :in ?pid
     :where
     [?p :person/name ?name]
     [?p :person/age ?age]
     [?p :person/children ?child]
     [?child :child/order ?child-idx]]

    (fn [db results] ;; this is kind of gross, we could write a utility function tho
     (let [[name age & _] (first results)]
       [:h1 name
        [:div "They are " [:span.age age] " years old"]
        [:div.children
         (for [[_ _ _ child] (sort-by fourth results)]
           (PersonCard db child))]])))


  )

