(ns factui.facts
  (:require #?(:clj [clojure.core :as core]
               :cljs [cljs.core :as core]))
  (:refer-clojure :exclude [type ancestors]))

(defrecord Datom [e a v])
(defrecord Operation [op args])
(defrecord TempidBinding [tempid eid])
(defrecord Attribute [ident type card-one? identity?])

(def hierarchy (make-hierarchy))

#?(:clj
   (defn type [obj]
     (case (.getName (core/type obj))
       "factui.facts.Datom" ::datom
       "factui.facts.Operation" ::operation
       "factui.facts.TempidBinding" ::tempid-binding
       "factui.facts.Attribute" ::attribute)))

#?(:cljs
   (defn type [obj]
     (cond
       (instance? Operation obj) ::operation
       (instance? Datom obj) ::datom
       (instance? TempidBinding obj) ::tempid-binding
       (instance? Attribute obj) ::attribute)))

(defn ancestors [type]
  (core/ancestors hierarchy type))
