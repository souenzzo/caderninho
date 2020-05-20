(ns br.com.souenzzo.caderninho
  (:require [br.com.souenzzo.caderninho.session :as session]
            [br.com.souenzzo.caderninho.todo :as todo]
            [br.com.souenzzo.caderninho.user :as user]
            [clojure.spec.alpha :as s]
            [com.rpl.specter :as sp]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [br.com.souenzzo.caderninho.query :as query]
            [com.wsscode.pathom.trace :as pt]
            [hiccup2.core :as h]
            [io.pedestal.http :as http]
            [net.molequedeideias.inga :as inga]
            [net.molequedeideias.inga-bootstrap.page :as bs.page]
            [net.molequedeideias.inga-bootstrap.ui :as bs.ui]
            [net.molequedeideias.inga.pedestal :as inga.pedestal]
            [spec-coerce.core :as sc]
            [io.pedestal.http.csrf :as csrf])
  (:import (java.net URLEncoder)
           (java.nio.charset StandardCharsets)))

(set! *warn-on-reflection* true)

(s/def :edn-query-language.pagination/first-element-index integer?)
(s/def :edn-query-language.pagination/elements-per-page integer?)

(def pages
  [{::inga.pedestal/path       "/"
    ::inga.pedestal/route-name ::index
    ::inga/head                {}
    ::inga/map-params          {:edn-query-language.pagination/elements-per-page :n}
    ::inga/body                {:>/all-todos {::inga/display-properties [:app.todo/id
                                                                         :app.user/username
                                                                         :app.todo/note
                                                                         `todo/delete]
                                              ::inga/->query            `inga/content->table-query
                                              ::inga/->data             `inga/data->table
                                              ::inga/->ui               `bs.ui/ui-table
                                              ::inga/join-key           ::query/all-todos}
                                :>/new-todo  {::inga/mutation `todo/new-todo
                                              ::inga/->query  `inga/content->form-query
                                              ::inga/->data   `inga/data->form
                                              ::inga/->ui     `bs.ui/ui-form}}
    ::inga/->query             `bs.page/->query
    ::inga/->data              `bs.page/->tree
    ::inga/->ui                `bs.page/->ui}
   {::inga.pedestal/path       "/sessions"
    ::inga.pedestal/route-name ::sessions
    ::inga/head                {}
    ::inga/body                {:>/login    {::inga/mutation `session/login
                                             ::inga/->query  `inga/content->form-query
                                             ::inga/->data   `inga/data->form
                                             ::inga/->ui     `bs.ui/ui-form}
                                :>/sessions {::inga/display-properties [:app.session/id
                                                                        :app.session/values
                                                                        :app.user/username]
                                             ::inga/->query            `inga/content->table-query
                                             ::inga/->data             `inga/data->table
                                             ::inga/->ui               `bs.ui/ui-table
                                             ::inga/join-key           ::query/all-sessions}
                                :>/users    {::inga/display-properties [:app.user/id
                                                                        :app.user/username
                                                                        ::user/session-count]
                                             ::inga/->query            `inga/content->table-query
                                             ::inga/->data             `inga/data->table
                                             ::inga/->ui               `bs.ui/ui-table
                                             ::inga/join-key           ::query/all-users}
                                :>/create   {::inga/mutation `session/create-user
                                             ::inga/->query  `inga/content->form-query
                                             ::inga/->data   `inga/data->form
                                             ::inga/->ui     `bs.ui/ui-form}}
    ::inga/->query             `bs.page/->query
    ::inga/->data              `bs.page/->tree
    ::inga/->ui                `bs.page/->ui}])


(defn service
  [env]
  (let [indexes (pc/register {}
                             (concat
                               pc/connect-resolvers
                               [pc/index-explorer-resolver
                                (pc/resolver
                                  `mutation-prefix
                                  {::pc/output [::inga/mutation-prefix]}
                                  (fn [{::csrf/keys [anti-forgery-token]} _]
                                    {::inga/mutation-prefix (str "/mutation/" (URLEncoder/encode (str anti-forgery-token)
                                                                                                 (str StandardCharsets/UTF_8)))}))]
                               (query/register)
                               (user/register)
                               (todo/register)
                               (session/register)))
        ref-indexes (atom indexes)
        parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/indexes ref-indexes})
                                       pt/trace-plugin]
                          ::p/mutate  pc/mutate})
        on-request (fn [req]
                     (merge req
                            env
                            {::p/reader               [p/map-reader
                                                       pc/reader3
                                                       pc/open-ident-reader
                                                       p/env-placeholder-reader]
                             ::p/entity               (atom {})
                             ::pc/indexes             @ref-indexes
                             ::p/placeholder-prefixes #{">"}}))]
    {::inga.pedestal/on-request           on-request
     ::inga.pedestal/api-path             "/api"
     ::inga.pedestal/form-mutation-prefix "/mutation/:csrf"
     ::inga.pedestal/indexes              indexes
     ::inga.pedestal/parser               parser
     ::inga.pedestal/read-token           ::session/read-token
     ::inga.pedestal/session-key-ident    ::session/session-key
     ::inga.pedestal/session-data-ident   ::session/session-values
     ::inga.pedestal/session-write-sym    `session/write-sesison
     ::inga.pedestal/pages                pages
     ::inga.pedestal/page->query          (fn [{:keys [query-params]
                                                :as   env} {::inga.pedestal/keys [route-name] :as page}]
                                            (let [{::session/keys [authed?]} (parser env [::session/authed?])
                                                  show-login? (and (not authed?)
                                                                   (= route-name ::index))
                                                  page (cond-> (merge env page)
                                                               (not authed?) (update ::inga/body dissoc :>/create :>/new-todo)
                                                               show-login?
                                                               (assoc ::inga/body {:>/login {::inga/mutation `session/login
                                                                                             ::inga/->query  `inga/content->form-query
                                                                                             ::inga/->data   `inga/data->form
                                                                                             ::inga/->ui     `bs.ui/ui-form}}))]
                                              (assoc page
                                                ::inga.pedestal/query [{:>/body (bs.page/->query (merge env page
                                                                                                        {::inga/default-params (sc/coerce-structure query-params)}))}])))
     ::inga.pedestal/result->tree         (fn [env {:>/keys [head body]}]
                                            {::head []
                                             ::body (bs.page/->tree env (sp/setval (sp/walker #{::p/not-found})
                                                                                   sp/NONE
                                                                                   body))})
     ::inga.pedestal/tree->ui             (fn [env {::keys [head body]}]
                                            [:html
                                             [:head
                                              (bs.page/std-head
                                                {::inga/title   "Caderninho"
                                                 ::inga/favicon (str "data:image/svg+xml;utf8,"
                                                                     (h/html
                                                                       [:svg
                                                                        {:xmlns   "http://www.w3.org/2000/svg"
                                                                         :viewBox "0 0 16 16"
                                                                         :width   "16"
                                                                         :height  "16"}
                                                                        [:text {:x "1" :y "13" :fill "royalblue"}
                                                                         "\uD83D\uDCD6"]]))})]
                                             [:body
                                              (bs.page/header {::inga/title        "Caderninho"
                                                               ::inga/current-user (-> (parser env [::session/current-username])
                                                                                       ::session/current-username
                                                                                       str)
                                                               ::inga/links [{::inga/href  "/"
                                                                              ::inga/label "home"}
                                                                             {::inga/href  "/sessions"
                                                                              ::inga/label "sessions"}]})
                                              (bs.page/->ui body)]])
     ::http/resource-path                 "META-INF/resources/webjars"
     ::http/secure-headers                {:content-security-policy-settings "script-src 'self'"}}))

