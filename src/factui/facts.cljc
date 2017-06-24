(ns factui.facts)

(defprotocol Transactional
  "A fact that gets cleaned up as the last step of a transaction.")

(defrecord Datom [e a v])

(defrecord Operation [op args]
  Transactional)

(defrecord TempidBinding [tempid eid]
  Transactional)

(defrecord Attribute [ident type card-many? identity?])