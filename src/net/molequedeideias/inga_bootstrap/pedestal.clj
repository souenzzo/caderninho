(ns net.molequedeideias.inga-bootstrap.pedestal
  (:require [spec-coerce.core :as sc]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.connect :as pc]
            [net.molequedeideias.inga :as inga]
            [net.molequedeideias.inga-bootstrap.ui :as bs.ui]
            [net.molequedeideias.inga-bootstrap.page :as bs.page]
            [ring.util.mime-type :as mime]
            [hiccup2.core :as h]))

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
  [{:keys [request]
    :as   context}]
  (let [query (inga/content->table-query request)]
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
  [{{::keys [data]} :request
    :keys           [request]
    :as             context}]
  (let [table (inga/data->table request data)]
    (assoc-in context [:request ::table] table)))

(defn enter-render
  [{{::keys [table]} :request
    :as              context}]
  (assoc context
    :response {:body   (bs.ui/ui-table table)
               :status 200}))

(defn stack
  [{::keys [parser indexes head header nav-menu update-request-fn]} page]
  [{:name  ::create-env
    :enter (fn [context]
             (-> context
                 (update :request
                         #(-> %
                              (assoc
                                ::pc/indexes indexes
                                ::head head
                                ::header header
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
   {:name ::render :enter enter-render}])

(defn routes
  [env pages]
  (set (for [{::inga/keys [path route-name handler]
              :as         page} pages]
         [path :get (or handler (stack env page))
          :route-name route-name])))
