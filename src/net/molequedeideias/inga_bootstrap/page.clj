(ns net.molequedeideias.inga-bootstrap.page
  (:require [net.molequedeideias.inga :as inga]
            [edn-query-language.core :as eql]))

(defn std-head
  [{::inga/keys [title resource-prefix favicon]}]
  (list
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:meta {:name "description" :content title}]
    [:meta {:charset "UTF-8"}]
    [:link {:rel "icon" :href favicon}]
    [:meta {:name "theme-color" :color "orange"}]
    [:link {:rel  "stylesheet"
            :href (str resource-prefix "/main.css")}]
    [:title title]))

(defn header
  [{::inga/keys [title links]}]
  [:header
   [:nav
    [:a {:href "/"}
     title]
    [:ul
     (for [{::inga/keys [href label active? icon]}
           links]
       [:li
        [:a (if-not active?
              {:href href})
         label]])]]])

(defn ->query
  [{::inga/keys [body] :as env}]
  (let [children (for [[k {::inga/keys [->query] :as data}] body
                       :let [->query (requiring-resolve ->query)
                             query (->query (merge env data))]]
                   (assoc (eql/query->ast query)
                     :type :join
                     :key k
                     :dispatch-key k
                     :query query))]
    (-> {:type     :root,
         :children children}
        eql/ast->query)))

(defn ->tree
  [{::inga/keys [body] :as env} data]
  (into {}
        (for [[k {::inga/keys [->data ->ui] :as opts}] body
              :let [->data (requiring-resolve ->data)]]
          [k {::->ui ->ui
              ::tree (->data (merge env opts)
                             (get data k))}])))

(defn ->ui
  [tree]
  [:main
   (for [[k {::keys [->ui tree]}] tree]
     [:section ((requiring-resolve ->ui)
                tree)])])
