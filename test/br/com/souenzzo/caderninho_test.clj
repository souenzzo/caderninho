(ns br.com.souenzzo.caderninho-test
  (:require [br.com.souenzzo.caderninho :as caderninho]
            [clojure.test :refer [deftest]]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [io.pedestal.http :as http]
            [io.pedestal.test :refer [response-for]]
            [midje.sweet :refer [fact contains => just]]
            [com.rpl.specter :as sp]
            [net.molequedeideias.inga.pedestal :as inga.pedestal]
            [next.jdbc :as jdbc]
            [net.molequedeideias.inga.transit :as transit]
            [clojure.string :as string]
            [hickory.core :as hickory]
            [io.pedestal.http.csrf :as csrf]
            [io.pedestal.interceptor :as interceptor])
  (:import (java.net URLEncoder URLDecoder)
           (java.nio.charset StandardCharsets)))

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
  (let [state (atom 42)
        register [(pc/resolver
                    `current-value
                    {::pc/output [::current-value]}
                    (fn [env input]
                      {::current-value @state}))
                  (pc/mutation
                    `app/inc
                    {}
                    (fn [_ _]
                      {::current-value (swap! state inc)}))]
        indexes (pc/register {}
                             register)
        ref-indexes (atom indexes)
        parser (p/parser {::p/mutate  pc/mutate
                          ::p/plugins [(pc/connect-plugin {::pc/indexes ref-indexes})]})
        service-fn (-> {::inga.pedestal/on-request           (fn [req]
                                                               (assoc req
                                                                 ::pc/indexes @ref-indexes
                                                                 ::p/reader [p/map-reader
                                                                             pc/reader2
                                                                             pc/open-ident-reader
                                                                             p/env-placeholder-reader]
                                                                 ::p/placeholder-prefixes #{">"}))
                        ::inga.pedestal/api-path             "/api"
                        ::inga.pedestal/form-mutation-prefix "/mutation/:csrf"
                        ::inga.pedestal/indexes              indexes
                        ::inga.pedestal/parser               parser
                        ::inga.pedestal/page->query          (fn [env {::inga.pedestal/keys [head body]}]
                                                               [{:>/head []}
                                                                {:>/body []}])
                        ::inga.pedestal/result->tree         (fn [env {:>/keys [head body]}]
                                                               {::head []
                                                                ::body []})
                        ::inga.pedestal/tree->ui             (fn [env {::keys [head body]}]
                                                               [:html
                                                                [:head]
                                                                [:body
                                                                 (::csrf/anti-forgery-token env)]])
                        ::inga.pedestal/pages                [{::inga.pedestal/path       "/empty"
                                                               ::inga.pedestal/route-name ::empty
                                                               ::inga.pedestal/head       {}
                                                               ::inga.pedestal/body       {}}]
                        ::http/enable-csrf                   {:read-token #(-> % :path-params :csrf str
                                                                               (URLDecoder/decode StandardCharsets/UTF_8))}
                        ::http/not-found-interceptor         (interceptor/interceptor
                                                               {:name  ::not-found
                                                                :leave (fn [{:keys [response request]
                                                                             :as   ctx}]
                                                                         (if (http/response? response)
                                                                           ctx
                                                                           (assoc ctx :response {:status 404
                                                                                                 :body   (pr-str
                                                                                                           (select-keys request
                                                                                                                        [:path-params
                                                                                                                         :path-info]))})))})
                        ::http/enable-session                {}}
                       inga.pedestal/default-interceptors
                       http/dev-interceptors
                       http/create-servlet
                       ::http/service-fn)
        {:keys [headers body]} (response-for service-fn :get "/empty")
        cookie (-> headers
                   (get "Set-Cookie")
                   first (string/split #"\;") first)
        csrf (-> body hickory/parse hickory/as-hickory (->> (sp/select-one [(sp/walker (comp #{:body} :tag))
                                                                            :content sp/FIRST]))
                 str
                 (URLEncoder/encode StandardCharsets/UTF_8))]
    (fact
      (-> (response-for service-fn :post "/api"
                        :body (transit/pr-str [::current-value]))
          :body
          transit/read-string
          ::current-value)
      => 42)

    (fact
      (-> (response-for service-fn :post "/api"
                        :body (transit/pr-str [::current-value]))
          :body
          transit/read-string
          ::current-value)
      => 42)
    (fact
      (response-for service-fn :post (str "/mutation/" csrf "/app/inc")
                    :headers {"Cookie" cookie})
      => "echo")
    (fact
      (-> (response-for service-fn :post "/api"
                        :body (transit/pr-str [::current-value]))
          :body
          transit/read-string
          ::current-value)
      => 43)))
