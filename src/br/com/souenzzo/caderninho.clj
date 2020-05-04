(ns br.com.souenzzo.caderninho
  (:require [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [hiccup2.core :as h]
            [io.pedestal.http :as http]
            [io.pedestal.http.csrf :as csrf]
            [net.molequedeideias.inga :as inga]
            [net.molequedeideias.inga-bootstrap.page :as bs.page]
            [net.molequedeideias.inga-bootstrap.pedestal :as bs.pedestal]
            [net.molequedeideias.inga-bootstrap.ui :as bs.ui]
            [net.molequedeideias.inga.pedestal :as inga.pedestal]
            [next.jdbc :as jdbc])
  (:import (java.util UUID)
           (java.nio.charset StandardCharsets)
           (java.net URLDecoder URLEncoder)))

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
     (fn [{::keys [sessions]} {::keys [session-key]}]
       {::session-values (get @sessions session-key)}))
   (pc/mutation
     `write-sesison
     {::pc/params [::session-key
                   ::session-values]}
     (fn [{::keys [sessions]} {::keys [session-key
                                       session-values]}]
       (let [session-key (or session-key
                             (str (UUID/randomUUID)))]
         (swap! sessions assoc session-key session-values)
         {::session-key session-key})))
   (pc/resolver
     `all-todos
     {::pc/output [::all-todos]}
     (fn [{::keys [conn]} input]
       (let [edges (for [{:app_todo/keys [id note]} (jdbc/execute! conn ["SELECT id, note FROM app_todo"])]
                     {:app-todo/id   id
                      :app-todo/note note})]
         {::all-todos {:edn-query-language.pagination/edges edges}})))
   (pc/resolver
     `all-sessions
     {::pc/output [::all-sessions]}
     (fn [{::keys [sessions]} input]
       (let [edges (for [[id values] @sessions]
                     {:app-session/id     id
                      :app-session/values (pr-str values)})]
         {::all-sessions {:edn-query-language.pagination/edges edges}})))
   (pc/resolver
     `mutation-prefix
     {::pc/output [::mutation-prefix]}
     (fn [{::csrf/keys [anti-forgery-token]} _]
       {::mutation-prefix (str "/mutation/" (URLEncoder/encode (str anti-forgery-token)
                                                               (str StandardCharsets/UTF_8)))}))
   (pc/mutation
     `new-todo
     {::pc/params [:app.todo/note]}
     (fn [{::keys [conn]} {:app.todo/keys [note]}]
       (jdbc/execute! conn ["INSERT INTO app_todo (id, note, authed) VALUES (DEFAULT, ?, 1)"
                            note])
       {}))])

(comment
  {::bs.pedestal/head     {::inga/title   "Caderninho"
                           ::inga/favicon (str "data:image/svg+xml;utf8,"
                                               (h/html
                                                 [:svg
                                                  {:xmlns   "http://www.w3.org/2000/svg"
                                                   :viewBox "0 0 16 16"
                                                   :width   "16"
                                                   :height  "16"}
                                                  [:text {:x "1" :y "13" :fill "royalblue"}
                                                   "\uD83D\uDCD6"]]))}
   ::bs.pedestal/header   {::inga/title "Caderninho"}
   ::bs.pedestal/nav-menu {::inga/links [{::inga/href  "/"
                                          ::inga/label "home"}]}}
  ::bs.pedestal/intercept-pages [{::bs.pedestal/show-when ::not-authed?
                                  ::inga/head             {}
                                  ::inga/body             {:>/form {::inga/mutation        `login
                                                                    ::inga/mutation-prefix "/mutations/"
                                                                    ::inga/->query         `inga/content->form-query
                                                                    ::inga/->data          `inga/data->form
                                                                    ::inga/->ui            `bs.ui/ui-form}}}])
(defn service
  [env]
  (let [indexes (pc/register {}
                             (concat
                               pc/connect-resolvers
                               (register)))
        sessions (atom {})
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
                             ::sessions               sessions
                             ::p/placeholder-prefixes #{">"}}))]
    (-> {::inga.pedestal/on-request           on-request
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
                                                ::inga/body                {:>/form  {::inga/ident-key          :>/a
                                                                                      ::inga/display-properties [:app-todo/id
                                                                                                                 :app-todo/note]
                                                                                      ::inga/->query            `inga/content->table-query
                                                                                      ::inga/->data             `inga/data->table
                                                                                      ::inga/->ui               `bs.ui/ui-table
                                                                                      ::inga/join-key           ::all-todos}
                                                                            :>/query {::inga/mutation              `new-todo
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
                                                ::inga/body                {:>/form {::inga/ident-key          :>/a
                                                                                     ::inga/display-properties [:app-session/id
                                                                                                                :app-session/values]
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
         ::http/secure-headers                {:content-security-policy-settings "script-src 'self'"}})))
