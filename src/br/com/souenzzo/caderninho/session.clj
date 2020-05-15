(ns br.com.souenzzo.caderninho.session
  (:require [next.jdbc :as jdbc]
            [br.com.souenzzo.caderninho.entity-db :as entity-db]
            [com.wsscode.pathom.connect :as pc]
            [io.pedestal.http.csrf :as csrf])
  (:import (java.util UUID)
           (java.nio.charset StandardCharsets)
           (java.net URLDecoder)))


(defn csrf->values
  [csrf]
  (if (string? csrf)
    {"__anti-forgery-token" csrf}
    {}))

(defn register
  []
  [(pc/mutation
     `create-user
     {::pc/params [:app.user/username]}
     (fn [{::csrf/keys      [anti-forgery-token]
           ::entity-db/keys [conn]} {:app.user/keys [username]}]
       (jdbc/with-transaction [tx conn]
         (jdbc/execute! tx ["INSERT INTO app_user (username) VALUES (?)"
                            username])
         (let [session-id (-> (jdbc/execute! tx ["SELECT id FROM app_session WHERE csrf = ?"
                                                 anti-forgery-token])
                              first
                              :app_session/id)
               user-id (-> (jdbc/execute! tx ["SELECT id FROM app_user WHERE username = ?"
                                              username])
                           first
                           :app_user/id)]
           (jdbc/execute! conn ["UPDATE app_session SET authed = ? WHERE id = ?"
                                user-id session-id])))
       {}))
   (pc/resolver
     `read-token
     {::pc/output [::read-token]}
     (fn [env input]
       {::read-token (-> env
                         :path-params
                         :csrf
                         str
                         (URLDecoder/decode (str StandardCharsets/UTF_8)))}))
   (pc/single-attr-resolver ::current-username ::authed? boolean)
   (pc/resolver
     `current-username
     {::pc/output [::current-username]}
     (fn [{::csrf/keys      [anti-forgery-token]
           ::entity-db/keys [conn]} _]
       (->> ["SELECT app_user.username
              FROM app_user
              JOIN app_session ON  app_session.csrf = ?
              WHERE app_session.authed = app_user.id"
             anti-forgery-token]
            (jdbc/execute! conn)
            first
            :app_user/username
            (hash-map ::current-username))))
   (pc/resolver
     `session-values
     {::pc/input  #{::session-key}
      ::pc/output [::session-values]}
     (fn [{::entity-db/keys [conn] :as env} {::keys [session-key]}]
       {::session-values (-> (when (string? session-key)
                               (jdbc/execute! conn ["SELECT csrf FROM app_session WHERE id = ?"
                                                    (UUID/fromString session-key)]))
                             first
                             :app_session/csrf
                             csrf->values)}))
   (pc/mutation
     `login
     {::pc/params [::current-username]}
     (fn [{::csrf/keys      [anti-forgery-token]
           ::entity-db/keys [conn]} {::keys [current-username]}]
       (jdbc/with-transaction [tx conn]
         (let [user-id (-> (jdbc/execute! tx ["SELECT id FROM app_user WHERE username = ?"
                                              current-username])
                           first
                           :app_user/id)
               session-id (-> (jdbc/execute! tx ["SELECT id FROM app_session WHERE csrf = ?"
                                                 anti-forgery-token])
                              first
                              :app_session/id)]
           (jdbc/execute! conn ["UPDATE app_session SET authed = ? WHERE id = ?"
                                user-id session-id]))
         {})))
   (pc/resolver
     `session-id->user-id
     {::pc/input  #{:app.session/id}
      ::pc/output [:app.user/id]}
     (fn [{::entity-db/keys [conn]} {:app.session/keys [id]}]
       (some->> (jdbc/execute! conn ["SELECT authed FROM app_session WHERE id = ?" id])
                first
                :app_session/authed
                (hash-map :app.user/id))))
   (pc/mutation
     `write-sesison
     {::pc/params [::session-key
                   ::session-values]}
     (fn [{::entity-db/keys [conn]} {::keys [session-key
                                             session-values]}]
       (let [session-key (if (string? session-key)
                           (UUID/fromString ^String session-key)
                           (UUID/randomUUID))]
         (jdbc/execute! conn ["INSERT INTO app_session (id, csrf) VALUES (?, ?)"
                              session-key (get session-values "__anti-forgery-token")])
         {::session-key session-key})))])