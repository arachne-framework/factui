 (ns dev
   (:require [clara.rules :refer [defrule] :as c]))

(comment

  (require '[figwheel-sidecar.repl-api :as ra])

  ;; this will start figwheel and will start autocompiling the builds specified in `:builds-ids`
  (ra/start-figwheel!)


  (ra/stop-figwheel!)



  )