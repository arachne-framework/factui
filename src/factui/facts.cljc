(ns factui.facts
  (:require #?(:clj [clojure.core :as core]
               :cljs [cljs.core :as core])))

(defrecord Datom [e a v])

(defrecord Operation [op args])

(defrecord TempidBinding [tempid eid])

(defrecord Attribute [ident type card-one? identity?])