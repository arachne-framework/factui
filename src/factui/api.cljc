(ns factui.api
  (:require [factui.session :as session]
            [factui.store :as store]
            [factui.compiler :as comp]
            #?(:clj [clara.rules :as cr]
               :cljs [clara.rules :as cr :include-macros true])
            #?(:clj [clara.rules.compiler :as com])
            [factui.specs :as fs]
            [factui.specs.clara :as cs]
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

#?(:clj
   (defmacro defquery
     "Define a Clara query using Datomic-style Datalog syntax."
     [& args]
     (let [input (s/conform ::fs/defquery-args args)
           output (comp/compile input comp/compile-defquery)
           clara (s/unform ::cs/defquery-args output)
           inputs (vec (::cs/query-params output))
           name (symbol (name (::cs/name output)))]
       `(do
          (cr/defquery ~@clara)
          ~(if (com/compiling-cljs?)
             `(set! ~name (assoc ~name ::inputs ~inputs))
             `(alter-var-root (var ~name) assoc ::inputs ~inputs))))))

(defn query
  "Run a FactUI query"
  [session query & args]
  (let [inputs (::factui.api/inputs query)
        clara-args (interleave inputs args)]
    (apply cr/query session (:name query) clara-args)))

(comment

  (use 'clojure.pprint)

  (pprint
    (s/conform ::fs/defquery-args '(person-name
                                     "Find a person's name"
                                     [:find ?name
                                      :in ?id
                                      :where [?id :person/name ?name]])))


  (pprint
    (s/conform ::fs/defquery-args '(person-name
                                     "Find a person's name"
                                     {:find [?name]
                                      :in [?id]
                                      :where [[?id :person/name ?name]]})))

  (pprint
    (macroexpand-1 '(defquery person-name
                      "Find a person's name"
                      [:find ?name
                       :in ?id
                       :where [?id :person/name ?name]])))

  (defquery person-name
    "find a person"
    [:find ?name
     :in ?id
     :where [?id :person/name ?name]])

  (pprint person-name)



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

