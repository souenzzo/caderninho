(ns br.com.souenzzo.caderninho.query
  (:require [com.wsscode.pathom.connect :as pc]
            [br.com.souenzzo.caderninho.entity-db :as entity-db]
            [next.jdbc :as jdbc]
            [br.com.souenzzo.caderninho.session :as session]
            [br.com.souenzzo.caderninho.user :as user]
            [br.com.souenzzo.caderninho.todo :as todo]
            [com.wsscode.pathom.core :as p]
            [io.pedestal.http.csrf :as csrf])
  (:import (java.nio.charset StandardCharsets)
           (java.net URLEncoder)))


(defn register
  []
  [(pc/resolver
     `session/all-sessions
     {::pc/output [::all-sessions]}
     (fn [{::entity-db/keys [conn]} input]
       (let [edges (for [{:app_session/keys [id csrf]} (jdbc/execute! conn ["SELECT id, csrf FROM app_session"])]
                     {:app.session/id     id
                      :app.session/values (pr-str (session/csrf->values csrf))})]
         {::all-sessions edges})))
   (pc/resolver
     `user/all-users
     {::pc/output [::all-users]}
     (fn [{::entity-db/keys [conn]} input]
       (let [edges (for [{:app_user/keys [id]} (jdbc/execute! conn ["SELECT id FROM app_user"])]
                     {:app.user/id id})]
         {::all-users edges})))
   (pc/resolver
     `todo/all-todos
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
         {::all-todos                                        edges
          ::session/current-username                         current-username
          :edn-query-language.pagination/elements-per-page   elements-per-page
          :edn-query-language.pagination/first-element-index first-element-index})))
   (pc/resolver
     `mutation-prefix
     {::pc/output [::mutation-prefix]}
     (fn [{::csrf/keys [anti-forgery-token]} _]
       {::mutation-prefix (str "/mutation/" (URLEncoder/encode (str anti-forgery-token)
                                                               (str StandardCharsets/UTF_8)))}))])
