(ns net.molequedeideias.inga-bootstrap.pedestal
  (:require [spec-coerce.core :as sc]
            [com.wsscode.pathom.connect :as pc]
            [net.molequedeideias.inga :as inga]
            [net.molequedeideias.inga-bootstrap.page :as bs.page]
            [ring.util.mime-type :as mime]
            [hiccup2.core :as h]
            [edn-query-language.core :as eql]))

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
                                       [:html
                                        [:head
                                         (bs.page/std-head head)]
                                        [:body
                                         (bs.page/std-header header)
                                         (bs.page/nav-menu nav-menu)
                                         body]]))
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
                             :as                          ctx}]
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
