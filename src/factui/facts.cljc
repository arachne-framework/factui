(ns factui.facts)

(defrecord Datom [e a v])

(defn fact-type-fn
  "Clara's 'fact-type-fn' function for Datoms"
  [fact]
  (let [t (type fact)]
    (if (= Datom t)
      (.-a fact)
      t)))

(defn ancestors-fn
  "Claras 'ancestors-fn function' for Datoms"
  [type]
  (if (keyword? type)
    [Datom]
    (ancestors type)))