(ns br.com.souenzzo.caderninho-test
  (:require [br.com.souenzzo.caderninho :as caderninho]
            [clojure.test :refer [deftest]]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [cognitect.transit :as t]
            [hiccup2.core :as h]
            [io.pedestal.http :as http]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.interceptor.chain :as chain]
            [io.pedestal.test :refer [response-for]]
            [midje.sweet :refer [fact contains => just]]
            [net.molequedeideias.inga :as inga]
            [net.molequedeideias.inga.pedestal :as inga.pedestal]
            [net.molequedeideias.inga.page :as inga.page]
            [next.jdbc :as jdbc]
            [clojure.edn :as edn])
  (:import (java.io ByteArrayOutputStream)))

(deftest foo
  (let [parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/register (caderninho/register)})]})
        conn (jdbc/get-datasource {:jdbcUrl (System/getenv "JDBC_DATABASE_URL")})
        env {::p/reader               [p/map-reader
                                       pc/reader2
                                       pc/open-ident-reader
                                       p/env-placeholder-reader]
             ::p/placeholder-prefixes #{">"}
             ::caderninho/conn        conn}]
    (fact
      (parser env [::caderninho/all-todos])
      => {})))

(deftest http-default
  (let [register [(pc/constantly-resolver ::echo "echo")]
        indexes (pc/register {}
                             register)
        ref-indexes (atom indexes)
        parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/indexes ref-indexes})]})
        service-fn (-> {::inga.pedestal/on-request (fn [req]
                                                     (assoc req
                                                       ::pc/indexes @ref-indexes
                                                       ::p/reader [p/map-reader
                                                                   pc/reader2
                                                                   pc/open-ident-reader
                                                                   p/env-placeholder-reader]
                                                       ::p/placeholder-prefixes #{">"}))
                        ::inga.pedestal/api-path   "/api"
                        ::inga.pedestal/indexes    indexes
                        ::inga.pedestal/parser     parser
                        ::inga.pedestal/pages      [{::inga.pedestal/path       "/"
                                                     ::inga.pedestal/route-name ::index
                                                     ::inga.pedestal/head       {}
                                                     ::inga.pedestal/body       {}}]
                        ::http/enable-csrf         {}
                        ::http/enable-session      {}}
                       inga.pedestal/default-interceptors
                       http/dev-interceptors
                       http/create-servlet
                       ::http/service-fn)]
    (fact
      (-> (response-for service-fn :post "/api"
                        :body (inga.pedestal/pr-transit-str [::echo]))
          :body
          inga.pedestal/read-transit-string
          ::echo)
      => "echo")))
