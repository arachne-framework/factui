(ns factui.explore
  (:require [clara.rules :as c]
            [clara.rules.compiler :as cc]
            [clojure.spec.alpha :as s]
            [clojure.core.async :as a]))

(defrecord Hero [name baggage weapon])

;; what I need: a rule that updates multiple components state whenever it fires.

(def ^:dynamic *update-components*)

(c/defrule calculate-speed
  "Calculate the speed"
  [?hero <- Hero (< 50 baggage)]
  [?hero <- Hero (= ?baggage baggage)]
  [?hero <- Hero (= ?name name)]
  =>
  ;(println ?name "is encumbered!")
  ;(println "The weight is:" ?baggage)
  (*update-components* ?name ?baggage)
  )

;; TODO: the tools are here.
;; I just need some macros to define a component+rule at the same time
;; Whenever the rule fires, it needs to update the STATE of all component instances with a specific PROPERTY.

;; Something like this:
(fui/defc Hero [[?hero :hero/name ?name]
                [?hero :hero/weapon ?w]]
  [?hero]
  [:div.title ?name
   [:span (str "Weapon:" ?w)]])

;; Perhaps mediated by a defsub feature?

;; Parts
;; A store
;; A rule which stuffs data into the store (index by variable)
;; A
;;


;; Underlying representation
(def calculate-speed-underlying

  '{:ns-name factui.explore,
   :lhs
   [{:type factui.explore.Hero,
     :constraints [(< 50 baggage)],
     :fact-binding :?hero}
    {:type factui.explore.Hero,
     :constraints [(= ?name name)],
     :fact-binding :?hero}],
   :rhs (do (println ?name "is encumbered!")),
   :name "factui.explore/calculate-speed",
   :doc "Calculate the speed"})

#_(def find-hero-rule
  '{:lhs
   [{:type factui.explore.Hero,
     :constraints [(= ?name name)],
     :fact-binding :?hero}],
   :params #{:?name},
   :name "factui.explore/find-hero",
   :doc "Find a hero"})

;; Manual testing:
#_(def productions
  (reify cc/IRuleSource
    (load-rules [_]
      [calculate-speed find-hero-rule])))

(comment



  (def sess (-> (c/mk-session)
              (c/insert (->Hero "Luke" 51))
              (c/insert (->Hero "Chad" 99))))

  (binding [*update-components*
            (fn [name baggage]
              (println "updating THIS:"
                name baggage))]
    (c/fire-rules sess))


  (-> (c/mk-session)
    (c/insert (->Hero "Luke" 51 nil))
    (c/fire-rules)

    )

  (def sess (c/mk-session productions))





  ;; A sample rule
  {:ns-name factui.explore,
   :lhs
   [{:type factui.explore.Hero,
     :constraints [(< 50 baggage)],
     :fact-binding :?hero}
    {:type factui.explore.Hero,
     :constraints [(= ?name name)],
     :fact-binding :?hero}],
   :rhs (do (println ?name "is encumbered!")),
   :name "factui.explore/calculate-speed",
   :doc "Calculate the speed"}

  ;; A sample query
  {:lhs
   [{:type factui.explore.Hero,
     :constraints [(= ?name name)],
     :fact-binding :?hero}],
   :params #{:?name},
   :name "factui.explore/find-hero",
   :doc "Find a hero"}






  )