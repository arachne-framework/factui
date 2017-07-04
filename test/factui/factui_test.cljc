(ns factui.factui-test
  (:require
    [factui.api :as api]
    [factui.facts :as f]
    [clara.rules :as c.r]
    [clojure.pprint :refer [pprint]]
   [clara.rules.memory :as mem]
   #?(:clj
    [clojure.test :as t :refer [deftest is testing run-tests]])
   #?(:cljs [cljs.test :as t :refer-macros [deftest is testing run-tests]])
    [clara.rules :as c.r]))

(def test-schema
  [{:db/ident :person/id
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :person/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :person/likes
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/many}
   {:db/ident :person/age
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}])

(require '[clara.rules :as c.r])

#_(api/defquery names
  []
  [:find ?name ?age ?likes
   :where
   [?p :person/name ?name]
   [?p :person/age ?age]
   [?p :person/likes ?likes]
   ]
  )

(c.r/defquery names-raw
  []
  [factui.facts.Datom [{:keys [e a v]}] (= e ?p) (= a :person/name) (= v ?name)]
  [factui.facts.Datom [{:keys [e a v]}] (= e ?p) (= a :person/age) (= v ?age)]
  [factui.facts.Datom [{:keys [e a v]}] (= e ?p) (= a :person/likes) (= v ?likes)])


(require '[clara.rules.accumulators :as c.a])

(c.r/defquery likes-agg
  []
  [?likes-agg <- (c.a/distinct) :from
   [factui.facts.Datom [{:keys [e a v]}] (= e ?p) (= a :person/likes)]]
  )


(c.r/defquery age-agg
  []
  [?likes-agg <- (c.a/max :v) :from
   [factui.facts.Datom [{:keys [e a v]}] (= e ?p) (= a :person/age)]]
  )

(c.r/defquery jointest
  []
  [factui.facts.Datom [{:keys [e a v]}] (= e ?p1) (= a :person/name) (= v ?n1)]
  [factui.facts.Datom [{:keys [e a v]}] (= e ?p2) (= a :person/name) (= v ?n2)]

  )

(api/defsession base* 'factui.factui-test)

(def base (api/transact-all base* test-schema))

(deftest )

(def sesh (api/transact-all base [{:person/name "Luke"
                                   :person/age 32
                                   :person/likes ["Beer" "Cheese"]}
                                  {:person/name "Alex"
                                   :person/age 31
                                   :person/likes ["Wine" "Bread"]}
                                  ]))

(pprint
  (c.r/query sesh age-agg))


(comment

  (def qn (get (:query-nodes (.-rulebase sesh)) age-agg))

  (mem/get-tokens-all (.-memory sesh) qn)

  )


#_(query [session query params]
  (let [query-node (get-in rulebase [:query-nodes query])]
    (when (= nil query-node)
      (platform/throw-error (str "The query " query " is invalid or not included in the rule base.")))

    (->> (mem/get-tokens memory query-node params)

      ;; Get the bindings for each token and filter generate symbols.
      (map (fn [{bindings :bindings}]

             ;; Filter generated symbols. We check first since this is an uncommon flow.
             (if (some #(re-find #"__gen" (name %)) (keys bindings) )

               (into {} (remove (fn [[k v]] (re-find #"__gen"  (name k)))
                          bindings))
               bindings))))))


#_(#clara.rules.engine.Token{:matches [[#{#factui.facts.Datom{:e 581, :a :person/likes, :v "Cheese"}
                                        #factui.facts.Datom{:e 581, :a :person/likes, :v "Beer"}} 42]]
                           :bindings {:?p 581, :?likes-agg #{#factui.facts.Datom{:e 581, :a :person/likes, :v "Cheese"}
                                                             #factui.facts.Datom{:e 581, :a :person/likes, :v "Beer"}}}}
 #clara.rules.engine.Token{:matches [[#{#factui.facts.Datom{:e 582, :a :person/likes, :v "Bread"}
                                        #factui.facts.Datom{:e 582, :a :person/likes, :v "Wine"}} 42]],
                           :bindings {:?p 582, :?likes-agg #{#factui.facts.Datom{:e 582, :a :person/likes, :v "Bread"}
                                                             #factui.facts.Datom{:e 582, :a :person/likes, :v "Wine"}}}})


















