(ns factui.impl.rules
  "A Clara session wrapper which builds in Datalog semantics."
  (:require [clara.rules :as cr]
            [factui.impl.session :as session]
            [factui.impl.store :as store]
   #?(:clj [factui.facts :as f]
      :cljs [factui.facts :as f :refer [Datom]]))
  #?(:clj (:import [factui.facts Datom])))

(cr/defrule remove-transients
  {:salience -1000000000}
  [?d <- Datom [{:keys [a]}] (= a ?a)]
   =>
  (when (factui.impl.store/transient? @factui.impl.session/*store* ?a)
    (cr/retract! ?d)))