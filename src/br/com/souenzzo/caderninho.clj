(ns br.com.souenzzo.caderninho
  (:require [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [hiccup2.core :as h]
            [io.pedestal.http :as http]
            [io.pedestal.http.csrf :as csrf]
            [br.com.souenzzo.caderninho.session :as session]
            [net.molequedeideias.inga :as inga]
            [net.molequedeideias.inga-bootstrap.page :as bs.page]
            [net.molequedeideias.inga-bootstrap.ui :as bs.ui]
            [br.com.souenzzo.caderninho.entity-db :as entity-db]
            [net.molequedeideias.inga.pedestal :as inga.pedestal]
            [next.jdbc :as jdbc]
            [br.com.souenzzo.caderninho.todo :as todo]
            [br.com.souenzzo.caderninho.user :as user]
            [com.wsscode.pathom.trace :as pt]
            [com.rpl.specter :as sp]
            [clojure.spec.alpha :as s]
            [spec-coerce.core :as sc])
  (:import (java.net URLEncoder)
           (java.nio.charset StandardCharsets)))

(set! *warn-on-reflection* true)


(s/def :edn-query-language.pagination/first-element-index integer?)

(s/def :edn-query-language.pagination/elements-per-page integer?)


(defn register
  []
  [(pc/resolver
     `all-sessions
     {::pc/output [::all-sessions]}
     (fn [{::entity-db/keys [conn]} input]
       (let [edges (for [{:app_session/keys [id csrf]} (jdbc/execute! conn ["SELECT id, csrf FROM app_session"])]
                     {:app.session/id     id
                      :app.session/values (pr-str (csrf->values csrf))})]
         {::all-sessions {:edn-query-language.pagination/edges edges}})))
   (pc/resolver
     `all-users
     {::pc/output [::all-users]}
     (fn [{::entity-db/keys [conn]} input]
       (let [edges (for [{:app_user/keys [id]} (jdbc/execute! conn ["SELECT id FROM app_user"])]
                     {:app.user/id id})]
         {::all-users {:edn-query-language.pagination/edges edges}})))
   (pc/resolver
     `all-todos
     {::pc/params [::session/current-username
                   :edn-query-language.pagination/first-element-index
                   :edn-query-language.pagination/elements-per-page]
      ::pc/output [::all-todos]}
     (fn [{:keys            [parser]
           ::entity-db/keys [conn]
           :as              env} input]
       (let [{::session/keys [current-username]
              :edn-query-language.pagination/keys
                             [first-element-index
                              elements-per-page]
              :or            {first-element-index 0
                              elements-per-page   3}} (p/params env)
             current-username (or current-username
                                  (-> env
                                      (parser [::session/current-username])
                                      ::session/current-username))
             edges (->> ["SELECT app_todo.id
                          FROM app_todo
                          JOIN app_user ON app_todo.author = app_user.id
                          WHERE app_user.username = ?
                          AND app_todo.id > ?
                          ORDER BY app_todo.id
                          LIMIT ?"
                         current-username
                         first-element-index elements-per-page]
                        (jdbc/execute! conn)
                        (map (comp (partial hash-map :app.todo/id)
                                   :app_todo/id)))]
         {::all-todos {:edn-query-language.pagination/edges               edges
                       ::session/current-username                         current-username
                       :edn-query-language.pagination/elements-per-page   elements-per-page
                       :edn-query-language.pagination/first-element-index first-element-index}})))
   (pc/resolver
     `mutation-prefix
     {::pc/output [::mutation-prefix]}
     (fn [{::csrf/keys [anti-forgery-token]} _]
       {::mutation-prefix (str "/mutation/" (URLEncoder/encode (str anti-forgery-token)
                                                               (str StandardCharsets/UTF_8)))}))])


(defn service
  [env]
  (let [indexes (pc/register {}
                             (concat
                               pc/connect-resolvers
                               [pc/index-explorer-resolver]
                               (register)
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
     ::inga.pedestal/pages                [{::inga.pedestal/path       "/"
                                            ::inga.pedestal/route-name ::index
                                            ::inga/head                {}
                                            ::inga/map-params          {:edn-query-language.pagination/elements-per-page :n}
                                            ::inga/body                {:>/all-todos {::inga/ident-key          :>/a
                                                                                      ::inga/display-properties [:app.todo/id
                                                                                                                 :app.user/username
                                                                                                                 :app.todo/note]
                                                                                      ::inga/->query            `inga/content->table-query
                                                                                      ::inga/->data             `inga/data->table
                                                                                      ::inga/->ui               `bs.ui/ui-table
                                                                                      ::inga/join-key           ::all-todos}
                                                                        :>/new-todo  {::inga/mutation              `todo/new-todo
                                                                                      ::inga/mutation-prefix-ident ::mutation-prefix
                                                                                      ::inga/->query               `inga/content->form-query
                                                                                      ::inga/->data                `inga/data->form
                                                                                      ::inga/->ui                  `bs.ui/ui-form}}
                                            ::inga/->query             `bs.page/->query
                                            ::inga/->data              `bs.page/->tree
                                            ::inga/->ui                `bs.page/->ui}
                                           {::inga.pedestal/path       "/sessions"
                                            ::inga.pedestal/route-name ::sessions
                                            ::inga/head                {}
                                            ::inga/body                {:>/login    {::inga/mutation              `session/login
                                                                                     ::inga/mutation-prefix-ident ::mutation-prefix
                                                                                     ::inga/->query               `inga/content->form-query
                                                                                     ::inga/->data                `inga/data->form
                                                                                     ::inga/->ui                  `bs.ui/ui-form}
                                                                        :>/sessions {::inga/ident-key          :>/a
                                                                                     ::inga/display-properties [:app.session/id
                                                                                                                :app.session/values
                                                                                                                :app.user/username]
                                                                                     ::inga/->query            `inga/content->table-query
                                                                                     ::inga/->data             `inga/data->table
                                                                                     ::inga/->ui               `bs.ui/ui-table
                                                                                     ::inga/join-key           ::all-sessions}
                                                                        :>/users    {::inga/ident-key          :>/a
                                                                                     ::inga/display-properties [:app.user/id
                                                                                                                :app.user/username]
                                                                                     ::inga/->query            `inga/content->table-query
                                                                                     ::inga/->data             `inga/data->table
                                                                                     ::inga/->ui               `bs.ui/ui-table
                                                                                     ::inga/join-key           ::all-users}
                                                                        :>/create   {::inga/mutation              `session/create-user
                                                                                     ::inga/mutation-prefix-ident ::mutation-prefix
                                                                                     ::inga/->query               `inga/content->form-query
                                                                                     ::inga/->data                `inga/data->form
                                                                                     ::inga/->ui                  `bs.ui/ui-form}}
                                            ::inga/->query             `bs.page/->query
                                            ::inga/->data              `bs.page/->tree
                                            ::inga/->ui                `bs.page/->ui}]
     ::inga.pedestal/page->query          (fn [{:keys [query-params]
                                                :as   env} {::inga.pedestal/keys [route-name] :as page}]
                                            (let [{::session/keys [authed?]} (parser env [::session/authed?])
                                                  show-login? (and (not authed?)
                                                                   (= route-name ::index))
                                                  page (cond-> (merge env page)
                                                               (not authed?) (update ::inga/body dissoc :>/create :>/new-todo)
                                                               show-login?
                                                               (assoc ::inga/body {:>/login {::inga/mutation              `session/login
                                                                                             ::inga/mutation-prefix-ident ::mutation-prefix
                                                                                             ::inga/->query               `inga/content->form-query
                                                                                             ::inga/->data                `inga/data->form
                                                                                             ::inga/->ui                  `bs.ui/ui-form}}))]
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
                                              (bs.page/std-header {::inga/title        "Caderninho"
                                                                   ::inga/current-user (-> (parser env [::session/current-username])
                                                                                           ::session/current-username
                                                                                           str)})
                                              (bs.page/nav-menu {::inga/links [{::inga/href  "/"
                                                                                ::inga/label "home"}
                                                                               {::inga/href  "/sessions"
                                                                                ::inga/label "sessions"}]})
                                              (bs.page/->ui body)]])
     ::http/resource-path                 "META-INF/resources/webjars"
     ::http/secure-headers                {:content-security-policy-settings "script-src 'self'"}}))

