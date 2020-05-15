(ns br.com.souenzzo.caderninho.user
  (:require [next.jdbc :as jdbc]
            [br.com.souenzzo.caderninho.entity-db :as entity-db]
            [com.wsscode.pathom.connect :as pc]))

(defn register
  []
  [(pc/resolver
     `user-id->username
     {::pc/input  #{:app.user/id}
      ::pc/output [:app.user/username]}
     (fn [{::entity-db/keys [conn]} {:app.user/keys [id]}]
       (some->> (jdbc/execute! conn ["SELECT username FROM app_user WHERE id = ?" id])
                first
                :app_user/username
                (hash-map :app.user/username))))
   (pc/resolver
     `todo-id->user-id
     {::pc/input  #{:app.todo/id}
      ::pc/output [:app.user/id]}
     (fn [{::entity-db/keys [conn]} {:app.todo/keys [id]}]
       (comment
         (jdbc/execute!
           @conn
           ["SELECT *
             FROM app_todo
             WHERE author = ANY(?)"
            (int-array [1 3])]))
       (some->> (jdbc/execute! conn ["SELECT author FROM app_todo WHERE id = ?" id])
                first
                :app_todo/author
                (hash-map :app.user/id))))])