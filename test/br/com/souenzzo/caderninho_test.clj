(ns br.com.souenzzo.caderninho-test
  (:require [clojure.test :refer [deftest]]
            [br.com.souenzzo.caderninho :as caderninho]
            [midje.sweet :refer [fact contains => just]]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [next.jdbc :as jdbc]))

(deftest foo
  (let [parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/register (caderninho/register)})]})
        conn (jdbc/get-datasource {:jdbcUrl (System/getenv "JDBC_DATABASE_URL")})
        env {::p/reader               [p/map-reader
                                       pc/reader2
                                       pc/open-ident-reader
                                       p/env-placeholder-reader]
             ::p/placeholder-prefixes #{">"}
             ::caderninho/conn        conn}]
    (fact
      (parser env [::caderninho/all-todos])
      => {})))
