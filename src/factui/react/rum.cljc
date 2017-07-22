(ns factui.react.rum
  (:require [rum.core :as rum]))




(comment


  (defc User
    "Render a user in the list"
    [db ?user-id]

    [:find ?name ?age
     :in ?user-id
     :where
     [?user-id :person/name ?name]
     [?user-id :person/age ?age]]

    (render [results]


      )


    )

  )