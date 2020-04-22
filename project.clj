(defproject br.com.souenzzo/caderninho "0.1.0"
  :dependencies [[com.wsscode/pathom "2.2.31"]
                 [edn-query-language/eql "0.0.9"]
                 [hiccup/hiccup "2.0.0-alpha2"]
                 [io.pedestal/pedestal.jetty "0.5.7"]
                 [io.pedestal/pedestal.service "0.5.7"]
                 [org.clojure/clojure "1.10.1"]
                 [org.postgresql/postgresql "42.2.12"]
                 [org.webjars.npm/bootstrap "4.4.1"]
                 [org.webjars.npm/jquery "3.4.1"]
                 [org.webjars.npm/popper.js "1.16.1"]
                 [seancorfield/next.jdbc "1.0.424"]]
  :profiles {:test {:dependencies [[midje/midje "1.9.9"]
                                   [clj-kondo/clj-kondo "2020.04.05"]
                                   [hickory/hickory "0.7.1"]]}}
  :min-lein-version "2.0.0"
  :main br.com.souenzzo.caderninho)
