(ns br.com.souenzzo.caderninho-test
  (:require [br.com.souenzzo.caderninho :as caderninho]
            [clojure.test :refer [deftest]]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [io.pedestal.http :as http]
            [io.pedestal.test :refer [response-for]]
            [midje.sweet :refer [fact contains => just]]
            [net.molequedeideias.inga.pedestal :as inga.pedestal]
            [next.jdbc :as jdbc]
            [net.molequedeideias.inga.transit :as transit]))

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
        service-fn (-> {::inga.pedestal/on-request   (fn [req]
                                                       (assoc req
                                                         ::pc/indexes @ref-indexes
                                                         ::p/reader [p/map-reader
                                                                     pc/reader2
                                                                     pc/open-ident-reader
                                                                     p/env-placeholder-reader]
                                                         ::p/placeholder-prefixes #{">"}))
                        ::inga.pedestal/api-path     "/api"
                        ::inga.pedestal/indexes      indexes
                        ::inga.pedestal/parser       parser
                        ::inga.pedestal/page->query  (fn [env {::inga.pedestal/keys [head body]}]
                                                       [{:>/head []}
                                                        {:>/body []}])
                        ::inga.pedestal/result->tree (fn [env {:>/keys [head body]}]
                                                       {::head []
                                                        ::body []})
                        ::inga.pedestal/tree->ui     (fn [env {::keys [head body]}]
                                                       [:html
                                                        [:head]
                                                        [:body
                                                         "ok"]])
                        ::inga.pedestal/pages        [{::inga.pedestal/path       "/empty"
                                                       ::inga.pedestal/route-name ::empty
                                                       ::inga.pedestal/head       {}
                                                       ::inga.pedestal/body       {}}]
                        ::http/enable-csrf           {}
                        ::http/enable-session        {}}
                       inga.pedestal/default-interceptors
                       http/dev-interceptors
                       http/create-servlet
                       ::http/service-fn)]
    (fact
      (-> (response-for service-fn :post "/api"
                        :body (transit/pr-str [::echo]))
          :body
          transit/read-string
          ::echo)
      => "echo")
    (fact
      (response-for service-fn :get "/empty")
      => "echo")))
