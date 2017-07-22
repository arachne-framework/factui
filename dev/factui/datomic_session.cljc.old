(ns factui.datomic-session
  (:require [clara.rules :as cr]
            [clara.rules.engine :as eng]
            [clara.rules.listener :as l]))

(def ^:dynamic *db*)

(deftype DatomicListener []
  l/IPersistentEventListener
  (to-transient [listener] listener)
  l/ITransientEventListener
  (left-activate! [listener node tokens]
    listener)
  (left-retract! [listener node tokens]
    listener)
  (right-activate! [listener node elements]
    listener)
  (right-retract! [listener node elements]
    listener)
  (insert-facts! [listener facts]
    (println "l:insert" facts)
    listener)
  (alpha-activate! [listener node facts]
    listener)
  (insert-facts-logical! [listener node token facts]
    (println "l:insert logical" facts)
    listener)
  (retract-facts! [listener facts]
    (println "l:retract" facts)
    listener)
  (alpha-retract! [listener node facts]
    listener)
  (retract-facts-logical! [listener node token facts]
    (println "l:retract logical" facts)
    listener)
  (add-accum-reduced! [listener node join-bindings result fact-bindings]
    listener)
  (remove-accum-reduced! [listener node join-bindings fact-bindings]
    listener)
  (add-activations! [listener node activations]
    listener)
  (remove-activations! [listener node activations]
    listener)
  (fire-rules! [listener node]
    listener)
  (to-persistent! [listener]
    listener)
  )

(deftype DatomicSession [delegate]
  eng/ISession
  (insert [session facts]
    (println "inserting...")
    (DatomicSession. (eng/insert delegate facts)))

  (retract [session facts]
    (println "retracting...")
    (DatomicSession. (eng/retract delegate facts))
    )

  (fire-rules [session]
    (println "firing rules...")
    (binding [*db* 42]
      (DatomicSession. (eng/fire-rules delegate))))

  (fire-rules [session opts]
    (println "firing rules(2)...")
    (binding [*db* 42]
      (DatomicSession. (eng/fire-rules delegate opts))))

  (query [session query params]
    (println "querying...")
    (eng/query delegate query params))

  (components [session]
    (println "Components...")
    (eng/components delegate)))

(defrecord Fact [tag])

(use 'clojure.pprint)

(cr/defrule implication
  [Fact (= tag "X")]
  =>
  (println "current: " *db*)
  (cr/insert! (->Fact "Y")))

(cr/defquery all []
  [Fact (= tag ?tag)]
  )

(defn datomic-session [base]
  (let [components (eng/components base)
        components (update components :listeners conj (DatomicListener.))]
    (DatomicSession. (eng/assemble components))))

(cr/defsession base 'factui.datomic-session)



(comment

  (do

    (def s (datomic-session base))
    (def s (cr/insert s (->Fact "X")))
    (def s (cr/fire-rules s))
    (cr/query s all)
    (def s (cr/retract s (->Fact "X")))
    (def s (cr/fire-rules s))
    (cr/query s all)

    (->

      (cr/fire-rules)
      (cr/query all)

      ))

  )


