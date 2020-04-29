(ns net.molequedeideias.inga.pedestal
  (:require [io.pedestal.http :as http]
            [io.pedestal.interceptor.chain :as interceptor.chain]
            [io.pedestal.http.route.router :as router]
            [cognitect.transit :as t]
            [io.pedestal.interceptor.chain :as chain]
            [io.pedestal.http.cors :as http.cors]
            [io.pedestal.http.csrf :as http.csrf]
            [io.pedestal.http.secure-headers :as http.secure-headers]
            [io.pedestal.http.ring-middlewares :as http.ring-middlewares]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.http.body-params :as http.body-params]
            [hiccup2.core :as h]
            [net.molequedeideias.inga :as inga]
            [io.pedestal.http.route :as http.route]
            [clojure.string :as string]
            [edn-query-language.core :as eql]
            [com.wsscode.pathom.connect :as pc]
            [spec-coerce.core :as sc]
            [ring.util.mime-type :as mime]
            [net.molequedeideias.inga.transit :as transit]
            [clojure.java.io :as io]))

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
     ::post-router-interceptors (filter
                                  (comp
                                    #{::http.ring-middlewares/session
                                      ::http.body-params/body-params
                                      ::http.csrf/anti-forgery}
                                    :name)
                                  std-interceptors)}))

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
  [{::keys [pages]}
   post-router-interceptors]
  (for [{::keys [route-name path]} pages]
    [path :get (into []
                     cat
                     [post-router-interceptors
                      [(fn [req] {:status 200 :body (pr-str req)})]])
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
                                 (update-in ctx [:response :body] transit/pr-str))}
                       (fn [{::keys [tx]
                             :as    env}]
                         {:body   (parser env tx)
                          :status 200})]
       :route-name ::api-path]}))

(defn default-interceptors
  [{::keys [on-request]
    :as    service-map}]
  (let [{::keys [post-router-interceptors
                 pre-router-interceptors]} (std-interceptors service-map)
        routes (into #{}
                     cat
                     [(page-routes service-map post-router-interceptors)
                      (parser-routes service-map)])
        router (router-interceptor (assoc service-map ::http/routes routes))]
    (assoc service-map
      ::http/interceptors (vec (concat pre-router-interceptors
                                       [(interceptor/interceptor {:name  ::on-request
                                                                  :enter (fn [ctx]
                                                                           (update ctx :request on-request))})
                                        router])))))



(defn enter-params
  [{{::inga/keys [params-as]
     :keys       [query-params]} :request
    :as                          context}]
  (update-in context [:request ::inga/default-params]
             (fn [params]
               (merge params (sc/coerce-structure (into {}
                                                        (map (fn [[k v]]
                                                               [(get params-as k k)
                                                                v]))
                                                        query-params))))))

(defn enter-request->query
  [{{::keys [->query]} :request
    :keys              [request]
    :as                context}]
  (let [query (->query request)]
    (assoc-in context [:request ::query] query)))

(defn enter-fetch-data
  [{{::keys [parser query]} :request
    :keys                   [request]
    :as                     context}]
  (let [data (parser request query)]
    (assoc-in context [:request ::data] data)))

(defn leave-hiccup->html
  [{{:keys [body]}                  :response
    {::keys [head header nav-menu]} :request
    :as                             context}]
  (if body
    (assoc context
      :response {:headers {"Content-Type" (get mime/default-mime-types "html")}
                 :body    (str (h/html {:mode :html}
                                       (h/raw "<!DOCTYPE html>")
                                       [:html]))
                 :status  200})
    context))
(defn enter-data->table
  [{{::keys [data ->data]} :request
    :keys                  [request]
    :as                    context}]
  (let [table (->data request data)]
    (assoc-in context [:request ::table] table)))

(defn enter-render
  [{{::keys [table ->ui]} :request
    :as                   context}]
  (assoc context
    :response {:body   (->ui table)
               :status 200}))

(defn stack
  [{::keys [parser indexes head header nav-menu update-request-fn]}
   {::inga/keys [->data ->query ->ui]
    :as         page}]
  (let [->data (requiring-resolve ->data)
        ->query (requiring-resolve ->query)
        ->ui (requiring-resolve ->ui)]
    [{:name  ::create-env
      :enter (fn [context]
               (-> context
                   (update :request
                           #(-> %
                                (assoc
                                  ::pc/indexes indexes
                                  ::head head
                                  ::header header
                                  ::->data ->data
                                  ::->query ->query
                                  ::->ui ->ui
                                  ::nav-menu nav-menu
                                  ::parser parser)
                                (merge page)))))}
     {:name  ::update-request-fn
      :enter (fn [ctx] (update ctx :request update-request-fn))}
     {:name ::params :enter enter-params}
     {:name ::request->query :enter enter-request->query}
     {:name ::fetch-data :enter enter-fetch-data}
     {:name ::hiccup->html :leave leave-hiccup->html}
     {:name ::data->table :enter enter-data->table}
     {:name ::render :enter enter-render}]))

(defn routes
  [{::keys [mutation-prefix indexes parser update-request-fn]
    :as    env} pages]
  (set (conj (for [{::inga/keys [path route-name handler]
                    :as         page} pages]
               [path :get (or handler (stack env page))
                :route-name route-name])
             [(str mutation-prefix "*dispatch-key") :post
              [{:name  ::create-env
                :enter (fn [context]
                         (-> context
                             (update :request
                                     #(-> %
                                          (assoc
                                            ::pc/indexes indexes
                                            ::parser parser)))))}
               {:name  ::update-request-fn
                :enter (fn [ctx] (update ctx :request update-request-fn))}
               {:name  ::mutation
                :enter (fn [{{:keys [path-params params] :as env} :request
                             :as                                  ctx}]
                         (let [{:keys [dispatch-key]} path-params
                               mutation (symbol dispatch-key)
                               params (into {}
                                            (for [k (-> indexes (pc/mutation-data mutation) ::pc/params)]
                                              [k (get params (str (namespace k)
                                                                  "/"
                                                                  (name k)))]))
                               children []
                               ast {:type     :root,
                                    :children [{:dispatch-key mutation
                                                :key          mutation
                                                :params       params
                                                :type         :call,
                                                :query        (eql/ast->query {:type     :root
                                                                               :children children})
                                                :children     children}]}
                               result (parser env (eql/ast->query ast))
                               response {:status  303
                                         :headers {"Location" (-> env :headers (get "referer" "/"))}}]
                           (assoc ctx :response response)))}]
              :route-name ::mutation])))
