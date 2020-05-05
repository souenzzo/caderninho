(ns br.com.souenzzo.caderninho
  (:require [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [hiccup2.core :as h]
            [io.pedestal.http :as http]
            [io.pedestal.http.csrf :as csrf]
            [net.molequedeideias.inga :as inga]
            [net.molequedeideias.inga-bootstrap.page :as bs.page]
            [net.molequedeideias.inga-bootstrap.ui :as bs.ui]
            [net.molequedeideias.inga.pedestal :as inga.pedestal]
            [next.jdbc :as jdbc])
  (:import (java.util UUID)
           (java.nio.charset StandardCharsets)
           (java.net URLDecoder URLEncoder)))

(set! *warn-on-reflection* true)

(defn csrf->values
  [csrf]
  (if (string? csrf)
    {"__anti-forgery-token" csrf}
    {}))

(defn register
  []
  [(pc/resolver
     `read-token
     {::pc/output [::read-token]}
     (fn [env input]
       {::read-token (-> env
                         :path-params
                         :csrf
                         str
                         (URLDecoder/decode (str StandardCharsets/UTF_8)))}))
   (pc/resolver
     `session-values
     {::pc/input  #{::session-key}
      ::pc/output [::session-values]}
     (fn [{::keys [conn]} {::keys [session-key]}]
       {::session-values (-> (when (string? session-key)
                               (jdbc/execute! conn ["SELECT csrf FROM app_session WHERE id = ?"
                                                    (UUID/fromString session-key)]))
                             first
                             :app_session/csrf
                             csrf->values)}))
   (pc/mutation
     `write-sesison
     {::pc/params [::session-key
                   ::session-values]}
     (fn [{::keys [conn]} {::keys [session-key
                                   session-values]}]
       (let [session-key (if (string? session-key)
                           (UUID/fromString ^String session-key)
                           (UUID/randomUUID))]
         (jdbc/execute! conn ["INSERT INTO app_session (id, csrf) VALUES (?, ?)"
                              session-key (get session-values "__anti-forgery-token")])
         {::session-key session-key})))
   (pc/resolver
     `all-todos
     {::pc/output [::all-todos]}
     (fn [{::keys [conn]} input]
       (let [edges (for [{:app_todo/keys [id note author]} (jdbc/execute! conn ["SELECT id, note, author FROM app_todo"])]
                     {:app.todo/id     id
                      :app.todo/author {:app.user/id author}
                      :app.todo/note   note})]
         {::all-todos {:edn-query-language.pagination/edges edges}})))
   (pc/resolver
     `all-sessions
     {::pc/output [::all-sessions]}
     (fn [{::keys [conn]} input]
       (let [edges (for [{:app_session/keys [id csrf]} (jdbc/execute! conn ["SELECT id, csrf FROM app_session"])]
                     {:app.session/id     id
                      :app.session/values (pr-str (csrf->values csrf))})]
         {::all-sessions {:edn-query-language.pagination/edges edges}})))
   (pc/resolver
     `mutation-prefix
     {::pc/output [::mutation-prefix]}
     (fn [{::csrf/keys [anti-forgery-token]} _]
       {::mutation-prefix (str "/mutation/" (URLEncoder/encode (str anti-forgery-token)
                                                               (str StandardCharsets/UTF_8)))}))
   (pc/resolver
     `user-id->username
     {::pc/input  #{:app.user/id}
      ::pc/output [:app.user/username]}
     (fn [{::keys [conn]} {:app.user/keys [id]}]
       (some->> (jdbc/execute! conn ["SELECT username FROM app_user WHERE id = ?" id])
                first
                :app_user/username
                (hash-map :app.user/username))))
   (pc/resolver
     `todo-id->user-id
     {::pc/input  #{:app.todo/id}
      ::pc/output [:app.user/id]}
     (fn [{::keys [conn]} {:app.todo/keys [id]}]
       (some->> (jdbc/execute! conn ["SELECT author FROM app_todo WHERE id = ?" id])
                first
                :app_todo/author
                (hash-map :app.user/id))))
   (pc/resolver
     `session-id->user-id
     {::pc/input  #{:app.session/id}
      ::pc/output [:app.user/id]}
     (fn [{::keys [conn]} {:app.session/keys [id]}]
       (some->> (jdbc/execute! conn ["SELECT authed FROM app_session WHERE id = ?" id])
                first
                :app_session/authed
                (hash-map :app.user/id))))
   (pc/mutation
     `new-todo
     {::pc/params [:app.todo/note]}
     (fn [{::keys [conn]} {:app.todo/keys [note]}]
       (jdbc/execute! conn ["INSERT INTO app_todo (id, note, author) VALUES (DEFAULT, ?, 1)"
                            note])
       {}))
   (pc/mutation
     `login
     {::pc/params [:app.user/username]}
     (fn [{::csrf/keys [anti-forgery-token]
           ::keys      [conn]} {:app.user/keys [username]}]
       (jdbc/with-transaction [tx conn]
         (let [user-id (-> (jdbc/execute! tx ["SELECT id FROM app_user WHERE username = ?"
                                              username])
                           first
                           :app_user/id)
               session-id (-> (jdbc/execute! tx ["SELECT id FROM app_session WHERE csrf = ?"
                                                 anti-forgery-token])
                              first
                              :app_session/id)]
           (jdbc/execute! conn ["UPDATE app_session SET authed = ? WHERE id = ?"
                                user-id session-id]))
         {})))])

(defn service
  [env]
  (let [indexes (pc/register {}
                             (concat
                               pc/connect-resolvers
                               [pc/index-explorer-resolver]
                               (register)))
        ref-indexes (atom indexes)
        parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/indexes ref-indexes})]
                          ::p/mutate  pc/mutate})
        on-request (fn [req]
                     (merge req
                            env
                            {::p/reader               [p/map-reader
                                                       pc/reader2
                                                       pc/open-ident-reader
                                                       p/env-placeholder-reader]
                             ::p/placeholder-prefixes #{">"}}))]
    {::inga.pedestal/on-request           on-request
     ::inga.pedestal/api-path             "/api"
     ::inga.pedestal/form-mutation-prefix "/mutation/:csrf"
     ::inga.pedestal/indexes              indexes
     ::inga.pedestal/parser               parser
     ::inga.pedestal/read-token           ::read-token
     ::inga.pedestal/session-key-ident    ::session-key
     ::inga.pedestal/session-data-ident   ::session-values
     ::inga.pedestal/session-write-sym    `write-sesison
     ::inga.pedestal/pages                [{::inga.pedestal/path       "/"
                                            ::inga.pedestal/route-name ::index
                                            ::inga/head                {}
                                            ::inga/body                {:>/query {::inga/ident-key          :>/a
                                                                                  ::inga/display-properties [:app.todo/id
                                                                                                             :app.user/username
                                                                                                             :app.todo/note]
                                                                                  ::inga/->query            `inga/content->table-query
                                                                                  ::inga/->data             `inga/data->table
                                                                                  ::inga/->ui               `bs.ui/ui-table
                                                                                  ::inga/join-key           ::all-todos}
                                                                        :>/form  {::inga/mutation              `new-todo
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
                                            ::inga/body                {:>/form  {::inga/mutation              `login
                                                                                  ::inga/mutation-prefix-ident ::mutation-prefix
                                                                                  ::inga/->query               `inga/content->form-query
                                                                                  ::inga/->data                `inga/data->form
                                                                                  ::inga/->ui                  `bs.ui/ui-form}
                                                                        :>/query {::inga/ident-key          :>/a
                                                                                  ::inga/display-properties [:app.session/id
                                                                                                             :app.session/values
                                                                                                             :app.user/username]
                                                                                  ::inga/->query            `inga/content->table-query
                                                                                  ::inga/->data             `inga/data->table
                                                                                  ::inga/->ui               `bs.ui/ui-table
                                                                                  ::inga/join-key           ::all-sessions}}
                                            ::inga/->query             `bs.page/->query
                                            ::inga/->data              `bs.page/->tree
                                            ::inga/->ui                `bs.page/->ui}]
     ::inga.pedestal/page->query          (fn [env page]
                                            [{:>/body (bs.page/->query (merge env page))}])
     ::inga.pedestal/result->tree         (fn [env {:>/keys [head body]}]
                                            {::head []
                                             ::body (bs.page/->tree env body)})
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
                                              (bs.page/std-header {::inga/title "Caderninho"})
                                              (bs.page/nav-menu {::inga/links [{::inga/href  "/"
                                                                                ::inga/label "home"}
                                                                               {::inga/href  "/sessions"
                                                                                ::inga/label "sessions"}]})
                                              (bs.page/->ui body)]])
     ::http/resource-path                 "META-INF/resources/webjars"
     ::http/secure-headers                {:content-security-policy-settings "script-src 'self'"}}))

