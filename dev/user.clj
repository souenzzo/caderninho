(ns user
  (:require [br.com.souenzzo.caderninho.main :as caderninho.main]
            [next.jdbc :as jdbc]))

(defn -main
  [& _]
  (require 'br.com.souenzzo.caderninho
           'br.com.souenzzo.caderninho.main
           'net.molequedeideias.inga.pedestal
           :reload)
  (apply caderninho.main/-main _))

(def conn
  (delay
    (jdbc/get-datasource {:jdbcUrl (System/getenv "JDBC_DATABASE_URL")})))

(defn sql
  [& opts]
  (jdbc/execute! @conn opts))
