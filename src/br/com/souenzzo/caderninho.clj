(ns br.com.souenzzo.caderninho
  (:require [clojure.edn :as edn]
            [io.pedestal.http :as http]
            [io.pedestal.interceptor.helpers :as interceptor]
            [net.molequedeideias.inga-bootstrap.pedestal :as bs.pedestal]
            [net.molequedeideias.inga :as inga]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.connect :as pc]
            [hiccup2.core :as h]
            [ring.util.mime-type :as mime]
            [next.jdbc :as jdbc]
            [clojure.java.io :as io]
            [net.molequedeideias.inga-bootstrap.ui :as bs.ui])
  (:import (org.eclipse.jetty.servlet ServletContextHandler)
           (org.eclipse.jetty.server.handler.gzip GzipHandler)))

(defn context-configurator
  "Habilitando gzip nas respostas"
  [^ServletContextHandler context]
  (let [gzip-handler (GzipHandler.)]
    (.setExcludedAgentPatterns gzip-handler (make-array String 0))
    (.setGzipHandler context gzip-handler))
  context)

(defn register
  []
  [(pc/resolver
     `all-todos
     {::pc/output [::all-todos]}
     (fn [{::keys [conn]} input]
       (let [edges (for [{:app_todo/keys [id note]} (jdbc/execute! conn ["SELECT id, note FROM app_todo"])]
                     {:app-todo/id   id
                      :app-todo/note note})]
         {::all-todos {:edn-query-language.pagination/edges edges}})))])

(defonce state (atom nil))
(defn service
  [env]
  (let [indexes (pc/register {}
                             (concat
                               pc/connect-resolvers
                               (register)))
        ref-indexes (atom indexes)
        parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/indexes ref-indexes})]})
        routes (bs.pedestal/routes
                 {::bs.pedestal/parser   parser
                  ::bs.pedestal/indexes  indexes
                  ::bs.pedestal/head     {::inga/title   "Caderninho"
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
                  ::bs.pedestal/nav-menu {}
                  ::bs.pedestal/update-request-fn
                                         (fn [req]
                                           (merge req
                                                  env
                                                  {::p/reader               [p/map-reader
                                                                             pc/reader2
                                                                             pc/open-ident-reader
                                                                             p/env-placeholder-reader]
                                                   ::p/placeholder-prefixes #{">"}}))}
                 [{::inga/path               "/"
                   ::inga/route-name         ::index
                   ::inga/ident-key          :>/a
                   ::inga/display-properties [:app-todo/id
                                              :app-todo/note]
                   ::inga/->query            `inga/content->table-query
                   ::inga/->data             `inga/data->table
                   ::inga/->ui               `bs.ui/ui-table
                   ::inga/join-key           ::all-todos}])
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
                                                          :status  404}))))]
    (-> {::http/routes                routes
         ::http/resource-path         "META-INF/resources/webjars"
         ::http/container-options     {:h2c?                 true
                                       :context-configurator context-configurator}
         ::http/secure-headers        {:content-security-policy-settings "script-src 'self'"}
         ::http/enable-session        {:cookie-attrs {:same-site :strict}}
         ::http/enable-csrf           {}
         ::http/not-found-interceptor not-found-interceptor}
        http/default-interceptors)))

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
