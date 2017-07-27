(ns factui.impl.txdata
  "Tools for converting Datomic-style txdata to sets of Clara facts"
  (:require [factui.facts :as f]
            [clojure.spec.alpha :as s]))

(s/def ::txdata (s/coll-of ::tx-item :min-count 1))

(s/def ::entity-id (s/or :eid pos-int?
                         :tempid any?))

(s/def ::attr-name qualified-keyword?)

(s/def ::primitive-value (complement coll?))

(s/def ::fn qualified-keyword?)

(s/def ::list (s/cat :fn ::fn :args (s/* any?)))

(s/def ::tx-item (s/or :list ::list
                       :map ::map))

(s/def ::map (s/map-of ::attr-name (s/or :map ::map
                                         :coll (s/coll-of
                                                 (s/or :primitive ::primitive-value
                                                       :map ::map)
                                                 :min-count 1)
                                         :value ::primitive-value)
               :min-count 1))

(let [next (atom -10001)]
  (defn next-tempid
    "Return a unique tempid value"
    []
    (swap! next dec)))

(declare map->lists)

(defn- map-child
  "Generate a seq of list txdata for a map child of the given entity ID"
  [parent-eid parent-attr child-map]
  (let [[child-eid child-lists] (map->lists child-map)]
    (conj child-lists
      [:db/add parent-eid parent-attr child-eid])))

(defn- map->lists
  "Convert map txdata to a tuple of [eid list-txdata]"
  [txmap]
  (let [eid (or (second (:db/id txmap)) (next-tempid))
        txmap (dissoc txmap :db/id)]
    [eid
     (mapcat (fn [[attr [type value]]]
               (case type
                 :map (map-child eid attr value)
                 :coll (mapcat (fn [[type value]]
                                 (case type
                                   :primitive [[:db/add eid attr value]]
                                   :map (map-child eid attr value)))
                         value)
                 :value [[:db/add eid attr value]]))
       txmap)]))

(defn- to-list
  "Convert a conformed txitem to a seq of list-form txdata"
  [[type txitem]]
  (case type
    :list [(s/unform ::list txitem)]
    :map (second (map->lists txitem))))

(defn operations
  "Given Datomic-style txdata, convert to a set of operation tuples.

   If an entity ID is a positive integer, it is presumed to be a concrete
   entity ID, otherwise it will be treated as a tempid."
  [txdata]
  (let [conformed (s/conform ::txdata txdata)]
    (when (= ::s/invalid conformed) (s/assert* ::txdata txdata))
    (->> conformed
      (mapcat to-list))))