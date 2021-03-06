(ns net.molequedeideias.inga.pedestal
  (:require [clojure.java.io :as io]
            [com.wsscode.pathom.connect :as pc]
            [edn-query-language.core :as eql]
            [hiccup2.core :as h]
            [ring.middleware.session.store :as session.store]
            [com.rpl.specter :as sp]
            [io.pedestal.http :as http]
            [io.pedestal.http.body-params :as http.body-params]
            [io.pedestal.http.csrf :as http.csrf]
            [io.pedestal.http.ring-middlewares :as http.ring-middlewares]
            [io.pedestal.http.route :as http.route]
            [io.pedestal.interceptor :as interceptor]
            [net.molequedeideias.inga :as inga]
            [net.molequedeideias.inga.transit :as transit]
            [ring.util.mime-type :as mime]
            [spec-coerce.core :as sc]
            [ring.middleware.multipart-params :as multipart-params]))

(def ^:dynamic *request*)

(defn std-interceptors
  [service-map]
  (let [std-interceptors (->> (assoc service-map ::http/routes #{})
                              http/default-interceptors
                              ::http/interceptors)]
    {::pre-router-interceptors  (remove
                                  (comp
                                    #{::http.route/router
                                      ::http.ring-middlewares/session
                                      ::http.body-params/body-params
                                      ::http.csrf/anti-forgery}
                                    :name)
                                  std-interceptors)
     ::post-router-interceptors (map
                                  (fn [{:keys [enter leave name] :as i}]
                                    (if (= name ::http.ring-middlewares/session)
                                      (-> i
                                          (assoc :enter (fn [{:keys [request] :as ctx}]
                                                          (binding [*request* request]
                                                            (enter ctx))))
                                          (assoc :leave (fn [{:keys [request] :as ctx}]
                                                          (binding [*request* request]
                                                            (leave ctx)))))
                                      i))
                                  (filter
                                    (comp
                                      #{::http.ring-middlewares/session
                                        ::http.body-params/body-params
                                        ::http.csrf/anti-forgery}
                                      :name)
                                    std-interceptors))}))

(defn router-interceptor
  [service-map]
  (->> service-map
       http/default-interceptors
       ::http/interceptors
       (filter (comp
                 #{::http.route/router}
                 :name))
       first))

(defn page-routes
  [{::keys [page->query
            parser
            result->tree
            tree->ui
            pages]
    :as    service-map}
   post-router-interceptors]
  (for [{::keys [route-name path] :as page} pages
        :let [page-kw (partial keyword (str (namespace route-name)
                                            "."
                                            (name route-name)))]]
    [path :get (into []
                     cat
                     [post-router-interceptors

                      [{:name  (page-kw "merge-service-map")
                        :enter (fn [ctx]
                                 (update ctx :request merge service-map))}
                       {:name  (page-kw "page->query")
                        :enter (fn [ctx]
                                 (update ctx :request
                                         (fn [env]
                                           (merge env (page->query env page)))))}
                       {:name  (page-kw "query->result")
                        :enter (fn [ctx]
                                 (update ctx :request
                                         (fn [{::keys [query]
                                               :as    env}]
                                           (assoc env ::result (parser env query)))))}
                       {:name  (page-kw "result->tree")
                        :enter (fn [ctx]
                                 (update ctx :request
                                         (fn [{::keys [result]
                                               :as    env}]
                                           (assoc env ::tree (result->tree env result)))))}
                       {:name  (page-kw "tree->ui")
                        :enter (fn [ctx]
                                 (update ctx :request
                                         (fn [{::keys [tree]
                                               :as    env}]
                                           (assoc env ::ui (tree->ui env tree)))))}
                       {:name  (page-kw "ui->html")
                        :enter (fn [{{::keys [ui]} :request
                                     :as           ctx}]
                                 (let [body (str (h/html
                                                   {:mode :html}
                                                   (h/raw "<!DOCTYPE html>")
                                                   ui))]
                                   (assoc ctx :response
                                              {:headers {"Content-Length" (str (count (.getBytes body)))
                                                         "Content-Type"   (mime/default-mime-types "html")}
                                               :body    body
                                               :status  200})))}]])
     :route-name route-name]))

(defn parser-routes
  [{::keys [api-path parser]}]
  (when api-path
    #{[api-path :post [{:name  ::content-type
                        :enter (fn [ctx]
                                 (let [tx (-> ctx
                                              :request
                                              :body
                                              io/input-stream
                                              transit/read)]
                                   (assoc-in ctx [:request ::tx] tx)))
                        :leave (fn [ctx]
                                 (update-in ctx [:response :body] #(->> %
                                                                        (sp/setval (sp/walker fn?) sp/NONE)
                                                                        transit/pr-str)))}
                       (fn [{::keys [tx]
                             :as    env}]
                         {:body   (parser env tx)
                          :status 200})]
       :route-name ::api-path]}))

(defn mutation-routes
  [{::keys [multipart-params-opts parser
            indexes form-mutation-prefix]} post-router-interceptors]
  (let [multipart (http.ring-middlewares/multipart-params multipart-params-opts)]
    (when form-mutation-prefix
      (for [[sym data] (::pc/index-mutations indexes)]
        [(str form-mutation-prefix
              "/"
              (namespace sym)
              "/"
              (name sym))
         :post
         (vec (concat
                [multipart]
                post-router-interceptors
                [{:name  (keyword sym)
                  :enter (fn [ctx]
                           (let [tx `[(~sym ~(->> ctx :request :params (into {} (map (juxt (comp keyword key)
                                                                                           val)))
                                                  sc/coerce-structure))]]
                             (parser (:request ctx) tx)
                             (assoc ctx :response {:headers {"Location" (-> ctx :request :headers (get "referer" "/"))}
                                                   :status  303})))}]))
         :route-name (keyword sym)]))))

(defn session-store
  [{::keys [parser session-key-ident session-data-ident session-write-sym]}]
  (reify session.store/SessionStore
    (read-session [_ key]
      (-> (parser *request*
                  [{[session-key-ident key] [session-data-ident]}])
          (get-in [[session-key-ident key]
                   session-data-ident])))
    (write-session [_ key data]
      (-> (parser *request*
                  `[{(~session-write-sym ~{session-key-ident  key
                                           session-data-ident data})
                     ~[session-key-ident]}])
          (get-in [session-write-sym session-key-ident])))))


(defn default-interceptors
  [{::keys [on-request read-token parser routes session-key-ident session-data-ident session-write-sym]
    :as    service-map}]
  (let [service-map (cond-> service-map
                            read-token (assoc-in [::http/enable-csrf :read-token]
                                                 #(-> (parser % [read-token])
                                                      read-token))
                            (and session-key-ident session-data-ident session-write-sym)
                            (assoc-in [::http/enable-session :store]
                                      (session-store service-map)))
        {::keys [post-router-interceptors
                 pre-router-interceptors]} (std-interceptors service-map)
        routes (into (set routes)
                     cat
                     [(page-routes service-map post-router-interceptors)
                      (mutation-routes service-map post-router-interceptors)
                      (parser-routes service-map)])
        router (router-interceptor (assoc service-map ::http/routes routes))]
    (assoc service-map
      ::http/interceptors (vec (concat pre-router-interceptors
                                       [(interceptor/interceptor {:name  ::on-request
                                                                  :enter (fn [ctx]
                                                                           (update ctx :request on-request))})
                                        router])))))
