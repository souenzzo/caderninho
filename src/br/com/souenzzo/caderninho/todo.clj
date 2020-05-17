(ns br.com.souenzzo.caderninho.todo
  (:require [next.jdbc :as jdbc]
            [com.wsscode.pathom.connect :as pc]
            [br.com.souenzzo.caderninho.entity-db :as entity-db]
            [com.wsscode.pathom.core :as p]
            [io.pedestal.http.csrf :as csrf]
            [clojure.spec.alpha :as s]))

(s/def :app.todo/id number?)

(defn register
  []
  [(pc/resolver
     `todo-id->note
     {::pc/input  #{:app.todo/id}
      ::pc/output [:app.todo/note]}
     (fn [{::entity-db/keys [conn]} {:app.todo/keys [id]}]
       (some->> (jdbc/execute! conn ["SELECT note FROM app_todo WHERE id = ?" id])
                first
                :app_todo/note
                (hash-map :app.todo/note))))
   (pc/mutation
     `delete
     {::pc/params [:app.todo/id]}
     (fn [{::entity-db/keys [conn]} {:app.todo/keys [id]}]
       (jdbc/execute! conn ["DELETE FROM app_todo WHERE id = ?"
                            id])
       {}))
   (pc/mutation
     `new-todo
     {::pc/params [:app.todo/note]}
     (fn [{::csrf/keys      [anti-forgery-token]
           ::entity-db/keys [conn]} {:app.todo/keys [note]}]
       (jdbc/with-transaction [tx conn]
         (let [user-id (-> (jdbc/execute! tx ["SELECT authed FROM app_session WHERE csrf = ?"
                                              anti-forgery-token])
                           first
                           :app_session/authed)]
           (jdbc/execute! tx ["INSERT INTO app_todo (note, author) VALUES (?, ?)"
                              note user-id])))
       {}))])