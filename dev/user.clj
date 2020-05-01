(ns user
  (:require [br.com.souenzzo.caderninho.main :as caderninho.main]))

(defn -main
  [& _]
  (apply caderninho.main/-main _))
