(ns br.com.souenzzo.caderninho
  (:require [clojure.edn :as edn]
            [io.pedestal.http :as http]
            [io.pedestal.interceptor.helpers :as interceptor]
            [net.molequedeideias.inga-bootstrap.pedestal :as bs.pedestal]
            [net.molequedeideias.inga :as inga]
            [net.molequedeideias.inga.pedestal :as inga.pedestal]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.connect :as pc]
            [hiccup2.core :as h]
            [ring.util.mime-type :as mime]
            [next.jdbc :as jdbc]
            [clojure.java.io :as io]
            [net.molequedeideias.inga-bootstrap.ui :as bs.ui]
            [net.molequedeideias.inga-bootstrap.page :as bs.page]
            [io.pedestal.http.csrf :as csrf])
  (:import (org.eclipse.jetty.servlet ServletContextHandler)
           (org.eclipse.jetty.server.handler.gzip GzipHandler)
           (java.util UUID)
           (java.nio.charset StandardCharsets)
           (java.net URLDecoder)))

(defn context-configurator
  "Habilitando gzip nas respostas"
  [^ServletContextHandler context]
  (let [gzip-handler (GzipHandler.)]
    (.setExcludedAgentPatterns gzip-handler (make-array String 0))
    (.setGzipHandler context gzip-handler))
  context)

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
                     (assoc req
                       ::p/reader [p/map-reader
                                   pc/reader2
                                   pc/open-ident-reader
                                   p/env-placeholder-reader]
                       ::p/placeholder-prefixes #{">"}))]
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
         ::inga.pedestal/pages                [{::inga/path       "/"
                                                ::inga/route-name ::new2
                                                ::inga/head       {}
                                                ::inga/body       {:>/form  {::inga/ident-key          :>/a
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
                                                ::inga/->query    `bs.page/->query
                                                ::inga/->data     `bs.page/->tree
                                                ::inga/->ui       `bs.page/->ui}]
         ::http/resource-path                 "META-INF/resources/webjars"
         ::http/container-options             {:h2c?                 true
                                               :context-configurator context-configurator}
         ::http/secure-headers                {:content-security-policy-settings "script-src 'self'"}
         ::http/not-found-interceptor         not-found-interceptor}
        inga.pedestal/default-interceptors)))

(defn -main
  [& opts]
  (comment
    {"LEIN_VERSION"               "2.9.1",
     "HOME"                       "/app",
     "JAVA_OPTS"                  "-Xmx300m -Xss512k -XX:CICompilerCount=2 -Dfile.encoding=UTF-8 ",
     "LEIN_HOME"                  "/app/.lein",
     "DYNO"                       "web.1",
     "LEIN_NO_DEV"                "yes",
     "PKG_CONFIG_PATH"            "/app/.heroku/apt/usr/lib/x86_64-linux-gnu/pkgconfig:/app/.heroku/apt/usr/lib/i386-linux-gnu/pkgconfig:/app/.heroku/apt/usr/lib/pkgconfig:",
     "LIBRARY_PATH"               "/app/.heroku/apt/usr/lib/x86_64-linux-gnu:/app/.heroku/apt/usr/lib/i386-linux-gnu:/app/.heroku/apt/usr/lib:",
     "JVM_OPTS"                   "-Xmx300m -Xss512k -XX:CICompilerCount=2 -Dfile.encoding=UTF-8 ",
     "JDBC_DATABASE_USERNAME"     "xxxx",
     "JDBC_DATABASE_PASSWORD"     "xxx",
     "DATABASE_URL"               "postgres://xxx:xxxx@ec2.compute.amazonaws.com:5432/xxxx",
     "PATH"                       "/app/.heroku/apt/usr/bin:/app/.heroku/bin:/app/.jdk/bin:/app/.heroku/nodejs/bin:/app/.heroku/clj/bin:/app/.jdk/bin:/app/.lein/bin:/usr/local/bin:/usr/bin:/bin",
     "INCLUDE_PATH"               "/app/.heroku/apt/usr/include:",
     "RING_ENV"                   "production",
     "TRAMPOLINE_FILE"            "/tmp/lein-trampoline-tUdH5XuQw1QP9",
     "CPATH"                      "/app/.heroku/apt/usr/include:",
     "CPPPATH"                    "/app/.heroku/apt/usr/include:",
     "JDBC_DATABASE_URL"          "jdbc:postgresql://ec2.compute.amazonaws.com:5432/xxx?user=xxx&password=xxx&sslmode=require",
     "CLASSPATH"                  "/app/.lein/self-installs/leiningen-2.9.1-standalone.jar",
     "JAVA_HOME"                  "/app/.jdk",
     "SCREENDIR"                  "/app/.heroku/apt/var/run/screen",
     "PWD"                        "/app",
     "JAVA_TOOL_OPTIONS"          "-Xmx300m -Xss512k -XX:CICompilerCount=2 -Dfile.encoding=UTF-8 ",
     "LEIN_JAVA_CMD"              "java",
     "LD_LIBRARY_PATH"            "/app/.jdk/jre/lib/amd64/server:/app/.jdk/jre/lib/amd64:/app/.jdk/jre/../lib/amd64:/app/.heroku/apt/usr/lib/x86_64-linux-gnu:/app/.heroku/apt/usr/lib/i386-linux-gnu:/app/.heroku/apt/usr/lib:/app/.jdk/jre/lib/amd64/server:",
     "SPRING_DATASOURCE_USERNAME" "xxx",
     "SPRING_DATASOURCE_URL"      "jdbc:postgresql://ec2.compute.amazonaws.com:5432/xxx?user=xxx&password=xxx&sslmode=require",
     "LEIN_JVM_OPTS"              "-Xverify:none -XX:+TieredCompilation -XX:TieredStopAtLevel=1",
     "SHLVL"                      "1",
     "PYTHONPATH"                 "/app/.heroku/apt/usr/lib/python2.7/dist-packages",
     "SPRING_DATASOURCE_PASSWORD" "xxxx",
     "PORT"                       "13340",
     "TELEGRAM_API_TOKEN"         "222:xxxx"})
  (let [port (edn/read-string (System/getenv "PORT"))
        jdbc-url (System/getenv "JDBC_DATABASE_URL")
        ds (jdbc/get-datasource {:jdbcUrl jdbc-url})]
    (with-open [conn (jdbc/get-connection ds)]
      (jdbc/execute! conn [(slurp (io/resource "schema.sql"))]))
    (try
      (with-open [conn (jdbc/get-connection ds)]
        (jdbc/execute! conn ["INSERT INTO app_user (id, username)
                              VALUES (DEFAULT, ?)"
                             "souenzzo"]))
      (catch Throwable ex
        (println ex)))
    (swap! state (fn [st]
                   (when st
                     (http/stop st))
                   (-> (service {::conn (jdbc/get-connection ds)})
                       (assoc ::http/port port
                              ::http/type :jetty
                              ::http/join? false
                              ::http/host "0.0.0.0")
                       http/dev-interceptors
                       http/create-server
                       http/start)))))
