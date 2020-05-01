(ns br.com.souenzzo.caderninho.main
  (:require [io.pedestal.http :as http]
            [next.jdbc :as jdbc]
            [br.com.souenzzo.caderninho :as caderninho]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import (org.eclipse.jetty.server.handler.gzip GzipHandler)
           (org.eclipse.jetty.servlet ServletContextHandler)))

(defn context-configurator
  "Habilitando gzip nas respostas"
  [^ServletContextHandler context]
  (let [gzip-handler (GzipHandler.)]
    (.setExcludedAgentPatterns gzip-handler (make-array String 0))
    (.setGzipHandler context gzip-handler))
  context)

(defonce state (atom nil))

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
                   (-> (caderninho/service {::caderninho/conn (jdbc/get-connection ds)})
                       (assoc ::http/port port
                              ::http/type :jetty
                              ::http/join? false
                              ::http/container-options {:h2c?                 true
                                                        :context-configurator context-configurator}
                              ::http/host "0.0.0.0")
                       http/dev-interceptors
                       http/create-server
                       http/start)))))
