(ns factui.store
  "A datom store. For storing datoms."
  (:require [factui.facts :as f]))

(defprotocol Store
  (card-one? [this attribute]
    "Efficiently test if a given attribute is :db.cardinality/one")
  (insert [this datoms]
    "Insert the datoms into the store")
  (retract [this datoms]
    "Retract the specified datoms from the store")
  (has? [this datom]
    "Return true if the store already contains the specified datom")
  (replaces [this datom]
    "Return any datom that would be replaced by the given datom. Returns nil if the given datom would not replace any existing ones."))

(defn- clean
  "Given a map, an entity and attribute name, remove the entire attr if it is
   nil/empty, and the entire entity if it has no attrs."
  [m e a]
  (if (empty? (get-in m [e a]))
    (let [m (update m e dissoc a)]
      (if (empty? (get m e))
        (dissoc m e)
        m))
    m))

;;TODO: This could be made substantially faster, if necessary, using transients and/or reducers.
(defrecord SimpleStore [card-one-attrs m]
  Store
  (card-one? [this attr] (card-one-attrs attr))
  (insert [this datoms]
    (assoc this :m (reduce (fn [m {:keys [e a v]}]
                             (if (card-one? this a)
                               (assoc-in m [e a] v)
                               (update-in m [e a] (fnil conj #{}) v)))
                     m
                     datoms)))
  (retract [this datoms]
    (assoc this :m (reduce (fn [m {:keys [e a v]}]
                             (if (card-one? this a)
                               (-> m
                                 (update-in [e] dissoc a)
                                 (clean e a))
                               (-> m
                                 (update-in [e a] disj v)
                                 (clean e a))))
                     m
                     datoms)))
  (has? [this {:keys [e a v]}]
    (when-let [val (get-in this [:m e a])]
      (or (= val v) (and (set? val) (contains? val v)))))
  (replaces [this {:keys [e a]}]
    (when (card-one? this a)
      (when-let [v (get-in this [:m e a])]
        (f/->Datom e a v)))))

(defn store
  "Return an empty Store instance"
  [schema]
  (let [c1-attrs (set (keep
                        (fn [[attr attr-m]]
                          (when (= :db.cardinality/one (:db/cardinality attr-m))
                            attr))
                        schema))]
    (->SimpleStore c1-attrs {})))

