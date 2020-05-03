(ns user
  (:require [br.com.souenzzo.caderninho.main :as caderninho.main]))

(defn -main
  [& _]
  (require 'br.com.souenzzo.caderninho
           'br.com.souenzzo.caderninho.main
           :reload)
  (apply caderninho.main/-main _))
