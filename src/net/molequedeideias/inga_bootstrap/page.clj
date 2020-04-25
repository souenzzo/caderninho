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
    [:script {:src (str resource-prefix "/jquery/3.4.1/dist/jquery.slim.min.js")}]
    [:script {:src (str resource-prefix "/popper.js/1.16.1/dist/umd/popper.min.js")}]
    [:script {:src (str resource-prefix "/bootstrap/4.4.1/dist/js/bootstrap.min.js")}]
    [:link {:rel  "stylesheet"
            :href (str resource-prefix "/bootstrap/4.4.1/dist/css/bootstrap.min.css")}]
    [:link {:rel  "stylesheet"
            :href (str resource-prefix "/main.css")}]
    [:title title]))

(defn std-header
  [{::inga/keys [subtitle title
                 menu
                 current-user]}]
  [:header
   {:style {:display          "flex"
            :color            "white"
            :padding          "1rem"
            :justify-content  "space-between"
            :background-color "orange"}}
   [:a
    {:href "/"}
    [:strong subtitle]
    title]
   [:div
    {:class "dropdown"}
    [:button {:class         "btn dropdown-toggle"
              :type          "button"
              :data-toggle   "dropdown"
              :aria-haspopup "true"
              :aria-expanded "false"}
     current-user]
    [:div {:class           "dropdown-menu dropdown-menu-right"
           :aria-labelledby "dropdownMenuButton"}
     (for [{::inga/keys [link label]} menu]
       [:a {:class "dropdown-item"
            :href  link}
        label])]]])


(defn nav-menu
  [{::inga/keys [links]}]
  [:nav
   {:style {:display          "flex"
            :flex-wrap        "wrap"
            :background-color "orange"
            :justify-content  "space-around"}}
   (for [{::inga/keys [href label active? icon]}
         links]
     [:a (if active?
           {:style {:text-align    "center"
                    :border-bottom "0.1rem solid black"}}
           {:href  href
            :style {:text-align "center"}})
      (when icon
        [:img {:height "30rem"
               :width  "30rem"
               :margin "auto"
               :alt    label
               :src    icon}])
      [:div
       {:style {:padding-left  "1rem"
                :padding-right "1rem"}}
       label]])])

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
  (for [[k {::keys [->ui tree]}] tree]
    ((requiring-resolve ->ui)
     tree)))
