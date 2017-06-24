(ns factui.ui.demo
  (:require [rum.core :as rum]
            [clara.rules :as r :refer-macros [defquery]]
            [factui.rules :as fui :refer-macros [defrule #_defquery]]
            [cljs.core.async :as a])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

#_(fui/defrule2 test-rule
  "Test rule to prove that datoms work"
  [fui/Datom (= e ?book) (= a :book/title) (= v ?title)]
  =>
  (println "Found a book with title:" ?title))

#_(fui/defrule2 1 2 3)

#_(fui/defrule test-rule
  "Test rule to prove that datoms work"
  [?book :page/title ?title]
  =>
  (println "Found a book with title:" ?title))

#_(fui/defrule2)

(defrule rule-name
  "docstr"
  [?e :book/title ?title]
  [?e :book/author ?author]
  =>
  (println ?title ", by " ?author))

(defquery pages-q
  "Query to find a page"
  [:?id]
  ;[?id :page/title ?title]
  [fui/Datom (= e ?id) (= a :page/title) (= v ?title)]
  [fui/Datom (= e ?id) (= a :page/widgets) (= v ?widget)]


   )

(defrule pages-rule
  "docstr"
  [fui/Datom (= e 666) (= a ?title)]
  =>
  (println "Found entity 666:" ?title))

(rum/defcs Page < (rum/local '() ::q)
                 {:should-update (fn [_ state]
                                   (let [[session] (:rum/args state)
                                         prev-q (::q state)
                                         q (r/query @session pages-q :?id 666)]
                                     (when (not= @prev-q q)
                                       (reset! prev-q q)
                                       true)))}
  [state session update-ch]
  [:div.content (str @(::q state))])



(r/defsession the-session 'factui.ui.demo)

#_(-> the-session
    (r/insert
      (fui/->Datom 42 :book/title "My Story")
      (fui/->Datom 42 :book/author "Luke"))
    (r/insert
      (fui/->Datom 43 :book/title "My Other Story")
      (fui/->Datom 43 :book/author "Joe"))
    (r/fire-rules)
    )

(defn now []
  (.getTime (js/Date.)))

(defn busy
  [ms]
  (let [end-time (+ (now) ms)]
    (loop []
      (if (< (now) end-time)
        (recur)
        nil))))

(let [el (.getElementById js/document "fps")
      last (atom (now))
      ft (atom 0)]
  (defn update-fps
    []
    (let [current (now)
          delta (- current @last)]
      (swap! ft (fn [ft] (+ ft (/ (- delta ft) 20))))
      (reset! last current)
      (aset el "textContent" (str (.toFixed (/ 1000 @ft) 1))))))

(defn ^:export main
  "Initialize the app and start rendering"
  []
  (let [app-atom (atom the-session)
        dom-root (.getElementById js/document "app")
        update-ch (a/chan 64)
        render (fn render []
                 (js/requestAnimationFrame
                   #(do
                      (swap! app-atom r/fire-rules)
                      (update-fps)
                      (rum/mount (Page app-atom update-ch) dom-root)
                      (render))))]

    (swap! app-atom r/insert (fui/->Datom 666 :page/title "My Page"))
    (render)))