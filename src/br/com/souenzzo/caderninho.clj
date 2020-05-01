(ns br.com.souenzzo.caderninho
  (:require [io.pedestal.http :as http]
            [io.pedestal.interceptor.helpers :as interceptor]
            [net.molequedeideias.inga-bootstrap.pedestal :as bs.pedestal]
            [net.molequedeideias.inga :as inga]
            [net.molequedeideias.inga.pedestal :as inga.pedestal]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.connect :as pc]
            [hiccup2.core :as h]
            [ring.util.mime-type :as mime]
            [next.jdbc :as jdbc]
            [net.molequedeideias.inga-bootstrap.ui :as bs.ui]
            [net.molequedeideias.inga-bootstrap.page :as bs.page]
            [io.pedestal.http.csrf :as csrf])
  (:import (java.util UUID)
           (java.nio.charset StandardCharsets)
           (java.net URLDecoder)))

(defn register
  []
  (let [sessions (atom {})]
    [(pc/resolver
       `read-token
       {::pc/output [::read-token]}
       (fn [env input]
         {::read-token (-> env
                           :path-params
                           :csrf
                           str
                           (URLDecoder/decode StandardCharsets/UTF_8))}))
     (pc/resolver
       `session-values
       {::pc/input  #{::session-key}
        ::pc/output [::session-values]}
       (fn [_ {::keys [session-key]}]
         {::session-values (get @sessions session-key)}))
     (pc/mutation
       `write-sesison
       {::pc/params [::session-key
                     ::session-values]}
       (fn [_ {::keys [session-key
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
       `csrf-token
       {::pc/output [::csrf/anti-forgery-token]}
       (fn [{::csrf/keys [anti-forgery-token]} _]
         {::csrf/anti-forgery-token anti-forgery-token}))
     (pc/mutation
       `new-todo
       {::pc/params [:app.todo/note]}
       (fn [{::keys [conn]} {:app.todo/keys [note]}]
         (jdbc/execute! conn ["INSERT INTO app_todo (id, note, authed) VALUES (DEFAULT, ?, 1)"
                              note])
         {}))]))

(defonce state (atom nil))
(comment
  {::bs.pedestal/parser          parser
   ::bs.pedestal/indexes         indexes
   ::bs.pedestal/mutation-prefix "/mutations/"
   ::bs.pedestal/head            {::inga/title   "Caderninho"
                                  ::inga/favicon (str "data:image/svg+xml;utf8,"
                                                      (h/html
                                                        [:svg
                                                         {:xmlns   "http://www.w3.org/2000/svg"
                                                          :viewBox "0 0 16 16"
                                                          :width   "16"
                                                          :height  "16"}
                                                         [:text {:x "1" :y "13" :fill "royalblue"}
                                                          "\uD83D\uDCD6"]]))}
   ::bs.pedestal/header          {::inga/title "Caderninho"}
   ::bs.pedestal/nav-menu        {::inga/links [{::inga/href  "/"
                                                 ::inga/label "home"}]}}
  ::bs.pedestal/intercept-pages [{::bs.pedestal/show-when ::not-authed?
                                  ::inga/head             {}
                                  ::inga/body             {:>/form {::inga/mutation        `login
                                                                    ::inga/mutation-prefix "/mutations/"
                                                                    ::inga/mutation-token  `[(::csrf/anti-forgery-token {:pathom/as :__anti-forgery-token})]
                                                                    ::inga/->query         `inga/content->form-query
                                                                    ::inga/->data          `inga/data->form
                                                                    ::inga/->ui            `bs.ui/ui-form}}}])
(defn service
  [env]
  (let [indexes (pc/register {}
                             (concat
                               pc/connect-resolvers
                               (register)))
        ref-indexes (atom indexes)
        parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/indexes ref-indexes})]
                          ::p/mutate  pc/mutate})
        not-found-interceptor (interceptor/after
                                ::not-found
                                (fn [{:keys [response request]
                                      :as   ctx}]
                                  (if (http/response? response)
                                    ctx
                                    (assoc ctx :response {:body    (str (h/html
                                                                          {:mode :html}
                                                                          (h/raw "<!DOCTYPE html>")
                                                                          [:html
                                                                           [:head [:title "404"]]
                                                                           [:body
                                                                            (inga/show request)]]))
                                                          :headers {"Content-Type" (mime/default-mime-types "html")}
                                                          :status  404}))))
        on-request (fn [req]
                     (merge req
                            env
                            {::p/reader               [p/map-reader
                                                       pc/reader2
                                                       pc/open-ident-reader
                                                       p/env-placeholder-reader]
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
         ::inga.pedestal/pages                [{::inga.pedestal/path       "/"
                                                ::inga.pedestal/route-name ::new2
                                                ::inga/head                {}
                                                ::inga/body                {:>/form  {::inga/ident-key          :>/a
                                                                                      ::inga/display-properties [:app-todo/id
                                                                                                                 :app-todo/note]
                                                                                      ::inga/->query            `inga/content->table-query
                                                                                      ::inga/->data             `inga/data->table
                                                                                      ::inga/->ui               `bs.ui/ui-table
                                                                                      ::inga/join-key           ::all-todos}
                                                                            :>/query {::inga/mutation        `new-todo
                                                                                      ::inga/mutation-prefix "/mutations/"
                                                                                      ::inga/mutation-token  `[(::csrf/anti-forgery-token {:pathom/as :__anti-forgery-token})]
                                                                                      ::inga/->query         `inga/content->form-query
                                                                                      ::inga/->data          `inga/data->form
                                                                                      ::inga/->ui            `bs.ui/ui-form}}
                                                ::inga/->query             `bs.page/->query
                                                ::inga/->data              `bs.page/->tree
                                                ::inga/->ui                `bs.page/->ui}]
         ::http/resource-path                 "META-INF/resources/webjars"
         ::http/secure-headers                {:content-security-policy-settings "script-src 'self'"}
         ::http/not-found-interceptor         not-found-interceptor}
        inga.pedestal/default-interceptors)))
