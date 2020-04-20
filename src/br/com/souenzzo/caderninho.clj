(ns br.com.souenzzo.caderninho
  (:require [clojure.edn :as edn]
            [io.pedestal.http :as http]
            [clojure.pprint :as pprint]
            [io.pedestal.interceptor.helpers :as interceptor]
            [net.molequedeideias.inga-bootstrap.pedestal :as bs.pedestal]
            [net.molequedeideias.inga :as inga]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.connect :as pc]
            [hiccup2.core :as h])
  (:import (org.eclipse.jetty.servlet ServletContextHandler)
           (org.eclipse.jetty.server.handler.gzip GzipHandler)))

(defn context-configurator
  "Habilitando gzip nas respostas"
  [^ServletContextHandler context]
  (let [gzip-handler (GzipHandler.)]
    (.setExcludedAgentPatterns gzip-handler (make-array String 0))
    (.setGzipHandler context gzip-handler))
  context)

(defonce state (atom nil))
(defn service
  [_]
  (let [indexes (pc/register {}
                             (concat
                               pc/connect-resolvers
                               [(pc/resolver
                                  `foo
                                  {::pc/output [::foo]}
                                  (constantly
                                    {::foo {:edn-query-language.pagination/edges (map (partial hash-map :a) (range 10))}}))]))
        ref-indexes (atom indexes)
        parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/indexes ref-indexes})]})
        routes (bs.pedestal/routes
                 {::bs.pedestal/parser   parser
                  ::bs.pedestal/indexes  indexes
                  ::bs.pedestal/head     {::inga/title      "a"
                                          ::inga/favicon (str "data:image/svg+xml;utf8,"
                                                              (h/html
                                                                [:svg
                                                                 {:xmlns   "http://www.w3.org/2000/svg"
                                                                  :viewBox "0 0 16 16"
                                                                  :width   "16"
                                                                  :height  "16"}
                                                                 [:text {:x "1" :y "13" :fill "royalblue"}
                                                                  "\uD83D\uDCD6"]]))}
                  ::bs.pedestal/header   {::inga/title    "a"
                                          ::inga/subtitle "b"}
                  ::bs.pedestal/nav-menu {}
                  ::bs.pedestal/update-request-fn
                                         (fn [req]
                                           (assoc req
                                             ::p/reader [p/map-reader
                                                         pc/reader2
                                                         pc/open-ident-reader
                                                         p/env-placeholder-reader]
                                             ::p/placeholder-prefixes #{">"}))}
                 [{::inga/path               "/"
                   ::inga/route-name         ::index
                   ::inga/ident-key          :>/a
                   ::inga/display-properties [:a]
                   ::inga/join-key           ::foo}])]
    (-> {::http/routes                routes
         ::http/resource-path         "META-INF/resources/webjars"
         ::http/container-options     {:h2c?                 true
                                       :context-configurator context-configurator}
         ::http/secure-headers        {:content-security-policy-settings "script-src-elem 'self'"}

         ::http/not-found-interceptor (interceptor/after
                                        ::not-found
                                        (fn [{:keys [response request]
                                              :as   ctx}]
                                          (if (http/response? response)
                                            ctx
                                            (assoc ctx :response {:body    (binding [*print-namespace-maps* false]
                                                                             (with-out-str (pprint/pprint request)))
                                                                  :headers {"Content-Type" "text/plain"}
                                                                  :status  404}))))}

        http/default-interceptors)))

(defn -main
  [& opts]
  (let [port (edn/read-string (System/getenv "PORT"))]
    (swap! state (fn [st]
                   (when st
                     (http/stop st))
                   (-> (service {})
                       (assoc ::http/port port
                              ::http/type :jetty
                              ::http/join? false
                              ::http/host "0.0.0.0")
                       http/dev-interceptors
                       http/create-server
                       http/start)))))
