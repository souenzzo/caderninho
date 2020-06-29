(ns br.com.souenzzo.noni.easy
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [io.pedestal.log :as log]))

(defn tx!
  [{:keys [parser]
    :as   env} tx]
  (parser env tx))

(defn expand-routes
  [{::pc/keys [register]
    :as       service-map}]
  (let [{::pc/keys [index-mutations]
         :as       indexes} (pc/register {} register)
        ref-indexes (atom indexes)
        parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/indexes ref-indexes})]})
        env (assoc service-map
              ::pc/indexes indexes
              ::p/reader [p/map-reader
                          pc/reader3
                          pc/open-ident-reader
                          p/env-placeholder-reader]
              :parser parser
              ::p/placeholder-prefixes #{">"})
        +env {:name  ::+env
              :enter (fn [ctx]
                       (update ctx :request merge env))}
        routes (for [{::keys [path description]} (::routes (parser env [::routes]))]
                 [path :get [+env
                             (fn [req]
                               {:body   (pr-str
                                          (tx! req [description]))
                                :status 200})]
                  :route-name description])
        mutations (for [[sym data] index-mutations]
                    [(str "/" sym)
                     :post [+env
                            (fn [_]
                              {:body   "WIP"
                               :status 200})]
                     :route-name (keyword sym)])]
    (route/expand-routes (into #{}
                               cat
                               [routes
                                mutations]))))




(defonce state
         (atom {}))


(defn start
  [{::http/keys [port]
    :as         service-map}]
  (let [{::http/keys [port] :as service} (-> service-map
                                             (assoc ::http/type :jetty
                                                    ::http/port (or port 8080)
                                                    ::http/routes (fn []
                                                                    (expand-routes service-map))
                                                    ::http/join? false)
                                             http/default-interceptors
                                             http/create-server)]
    (swap! state (fn [st]
                   (some-> st (get port) http/stop)
                   (assoc st
                     port (http/start service))))))

(pc/defmutation new-note [_ {:app.note/keys [text]}]
  {::pc/sym    'app.note/new-note
   ::pc/params [:app.note/text]}
  (log/info :text text)
  {})

(pc/defresolver all-notes [_ _]
  {::pc/output [:app.note/all-notes]}
  {:app.note/all-notes [{:app.note/text "a"}
                        {:app.note/text "b"}]})

(pc/defresolver index [_ _]
  {::pc/output [::index]}
  {::index [{::mutation `app.note/new-note}
            {::display-properties [:app.note/text]
             ::join-key           :app.note/all-notes}]})

(pc/defresolver routes [_ _]
  {::pc/output [::routes]}
  {::routes [{::path        "/"
              ::description ::index}]})
(defn -main
  []
  (start {::pc/register [new-note
                         all-notes
                         index
                         routes]}))
