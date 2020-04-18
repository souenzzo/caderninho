(ns br.com.souenzzo.caderninho
  (:require [clojure.edn :as edn]
            [io.pedestal.http :as http]
            [clojure.pprint :as pprint]
            [ring.util.mime-type :as mime]))


(defonce state (atom nil))
(defn -main
  [& opts]
  (prn [:a (new java.util.Date)])
  (let [port (edn/read-string (System/getenv "PORT"))
        _ (prn [port (new java.util.Date)])
        handler (fn [req]
                  {:body    (binding [*print-namespace-maps* false]
                              (with-out-str (pprint/pprint req)))
                   :headers {"Content-Type" "text/plain"}
                   :status  200})
        service {::http/port   port
                 ::http/type   :jetty
                 ::http/join?  false
                 ::http/host   "0.0.0.0"
                 ::http/routes #{["/*path" :any handler
                                  :route-name ::index*]
                                 ["/" :any handler
                                  :route-name ::index]}}]
    (swap! state (fn [st]
                   (when st
                     (http/stop st))
                   (-> service
                       http/default-interceptors
                       http/create-server
                       http/start)))
    (prn [:xx (new java.util.Date)])))
