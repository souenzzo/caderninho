(ns br.com.souenzzo.noni.easy
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [io.pedestal.log :as log]
            [hiccup2.core :as h]
            [edn-query-language.core :as eql]
            [io.pedestal.http.body-params :as body-params]))

(defn dispatch!
  [{:keys [parser]
    :as   env} tx]
  (parser env tx))


(def html-response
  {:name  ::html-response
   :leave (fn [{:keys [response]
                :as   ctx}]
            (assoc ctx :response
                       (-> response
                           (update :body #(str
                                            (h/html
                                              {:lang :html}
                                              (h/raw "<!DOCTYPE html>")
                                              %)))
                           (update :headers assoc "Content-Type" "text/html; charset=utf-8"))))})

(def noni-reg
  [(pc/resolver `query
                {::pc/params [::display-properties
                              ::join-key]
                 ::pc/output [::query]}
                (fn [env input]
                  (let [{::keys [display-properties
                                 join-key]} (p/params env)
                        children (:children (eql/query->ast display-properties))
                        ast {:type         :join
                             :dispatch-key join-key
                             :key          join-key
                             :children     (remove (comp #{:call} :type)
                                                   children)}]
                    {::query {::ast ast
                              ::ui  (fn [data]
                                      (let [vs (get data join-key)]
                                        [:table
                                         [:thead
                                          (for [{:keys [dispatch-key]} children]
                                            [:tr (pr-str dispatch-key)])]
                                         [:tbody
                                          (for [v vs]
                                            [:tr
                                             (for [{:keys [dispatch-key params]} children]
                                               [:td
                                                (if (symbol? dispatch-key)
                                                  (let [{::keys [hidden]} params
                                                        {::pc/keys [params]} (pc/mutation-data env dispatch-key)]
                                                    [:form
                                                     {:action (str dispatch-key)
                                                      :method "POST"}
                                                     (for [{:keys [dispatch-key]} (-> params eql/query->ast :children)]
                                                       [:input
                                                        (cond-> {:name (str (namespace dispatch-key)
                                                                            "/"
                                                                            (name dispatch-key))}
                                                                (contains? v dispatch-key) (assoc :value (get v dispatch-key))
                                                                (contains? hidden dispatch-key) (assoc :hidden true))])
                                                     [:input {:type  "submit"
                                                              :value (str dispatch-key)}]])
                                                  (pr-str (get v dispatch-key)))])])]]))}})))
   (pc/resolver `mutation
                {::pc/params [::sym]
                 ::pc/output [::mutation]}
                (fn [env input]
                  (let [{::keys [sym]} (p/params env)
                        {::pc/keys [params]} (pc/mutation-data env sym)]
                    {::mutation {::ui (fn [data]
                                        [:form
                                         {:action (str sym)
                                          :method "POST"}
                                         (for [{:keys [dispatch-key]} (-> params
                                                                          eql/query->ast
                                                                          :children)]
                                           [:label
                                            (pr-str dispatch-key)
                                            [:input {:name (str (namespace dispatch-key)
                                                                "/"
                                                                (name dispatch-key))}]])
                                         [:input {:type  "submit"
                                                  :value (str sym)}]])}})))
   (pc/resolver `page-body
                {::pc/input  #{::route-name}
                 ::pc/output [::page-body]}
                (fn [env {::keys [route-name]}]
                  (let [data-query (get (dispatch! env [route-name]) route-name)
                        result (dispatch! env data-query)
                        data-query2 (->> (keep ::ast (vals result))
                                         (hash-map :type :root :children)
                                         eql/ast->query)
                        result2 (dispatch! env data-query2)]
                    {::page-body [:html
                                  [:head
                                   [:title (str route-name)]]
                                  [:body
                                   (for [[k {::keys [ui]}] result
                                         :when ui]
                                     (ui result2))]]})))])

(defn expand-routes
  [{::pc/keys [register]
    :as       service-map}]
  (let [{::pc/keys [index-mutations]
         :as       indexes} (pc/register {} (concat noni-reg
                                                    register))
        ref-indexes (atom indexes)
        parser (p/parser {::p/plugins [(pc/connect-plugin {::pc/indexes ref-indexes})]
                          ::p/mutate  pc/mutate})
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
        {::keys [routes]} (dispatch! env [::routes])
        pages (for [{::keys [path route-name]} routes]
                [path :get [+env
                            html-response
                            (fn [env]
                              {:body   (-> (dispatch! env [{[::route-name route-name] [::page-body]}])
                                           (get-in [[::route-name route-name]
                                                    ::page-body]))
                               :status 200})]
                 :route-name route-name])
        mutations (for [[sym data] index-mutations]
                    [(str "/" sym)
                     :post [+env
                            (body-params/body-params)
                            (fn [{:keys [form-params] :as env}]
                              (let [tx `[(~sym ~form-params)]
                                    _ (log/info :tx tx)
                                    result (dispatch! env tx)]
                                (log/info :result result)
                                {:headers {"Location" (-> env :headers (get "referer"))}
                                 :status  301}))]
                     :route-name (keyword sym)])]
    (-> (into #{}
              cat
              [pages
               mutations])
        route/expand-routes)))

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

;;;;;;;;;;;;;;;;;;;;
;; aplicação demo ;;
;;;;;;;;;;;;;;;;;;;;

;; ** parte da ui **

;; roteador do APP
(pc/defresolver routes [_ _]
  {::pc/output [::routes]}
  {::routes [;; Exite uma rota `/` e ela está descrita em `::index`
             {::path       "/"
              ::route-name ::index}]})

;; descrição de `::index`
(pc/defresolver index [_ _]
  {::pc/output [::index]}
  {::index `[;; vou exibir um com a mutation `app.note/new-note` e seus respectivos parametros
             (::mutation
               {::sym ~'app.note/new-note})
             ;; vou exibir o resultado de uma query em formato de tabela
             (::query
               {::display-properties [;; primeira coluna
                                      :app.note/text
                                      ;; segunda coluna vai ser uma mutation
                                      (~'app.note/delete-note {::hidden #{:app.note/text}})]
                ::join-key           :app.note/all-notes})]})

;; ** parte da 'regra de negocio' **

(pc/defmutation new-note [{::keys [note-db]} {:app.note/keys [text]}]
  {::pc/sym    'app.note/new-note
   ::pc/params [:app.note/text]}
  (swap! note-db conj {:app.note/text text})
  {})

(pc/defmutation delete-note [{::keys [note-db]} {:app.note/keys [text]}]
  {::pc/sym    'app.note/delete-note
   ::pc/params [:app.note/text]}
  (swap! note-db (fn [notes]
                   (remove (comp #{text} :app.note/text)
                           notes)))
  {})

(pc/defresolver all-notes [{::keys [note-db]} _]
  {::pc/output [:app.note/all-notes]}
  {:app.note/all-notes @note-db})

;; 'main' da app

(defonce note-db (atom [{:app.note/text "a"}
                        {:app.note/text "b"}]))

(defn -main
  []
  (start {::note-db     note-db
          ::pc/register [new-note
                         delete-note
                         all-notes
                         index
                         routes]}))
