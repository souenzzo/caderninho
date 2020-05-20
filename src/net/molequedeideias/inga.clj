(ns net.molequedeideias.inga
  (:require [edn-query-language.core :as eql]
            [com.wsscode.pathom.connect :as pc]))

(defn show
  [x]
  (cond
    (map? x) [:div
              {:style {:display "flex"}}
              [:span "{"]
              [:table
               [:tbody
                (for [[k v] x]
                  [:tr
                   [:th (show k)]
                   [:td
                    (show v)]])]]
              [:span
               {:style {:align-self "flex-end"}}
               "}"]]
    (set? x) [:div
              {:style {:display "flex"}}
              [:span "#{"]
              [:ul
               (for [el x]
                 [:li (show el)])]
              [:span
               {:style {:align-self "flex-end"}}
               "}"]]
    (coll? x) [:div
               {:style {:display "flex"}}
               [:span "["]
               [:ol
                {:start 0}
                (for [el x]
                  [:li (show el)])]
               [:span
                {:style {:align-self "flex-end"}}
                "]"]]
    (keyword? x) [:code
                  {:style {:background-color "fuchsia"}}
                  (pr-str x)]
    (string? x) [:code
                 {:style {:background-color "lightgreen"}}
                 (pr-str x)]
    (number? x) [:code
                 {:style {:background-color "lightblue"}}
                 (pr-str x)]
    (true? x) [:code
               {:style {:background-color "green"}}
               (pr-str x)]
    (false? x) [:code
                {:style {:background-color "red"}}
                (pr-str x)]
    (nil? x) [:code
              {:style {:background-color "blue"}}
              (pr-str x)]
    :else [:code
           {:style {:background-color "yellow"}}
           (pr-str x)]))

(defn distinct-by
  [pred]
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn
        ([] (rf))
        ([coll] (rf coll))
        ([coll el]
         (let [id (pred el)]
           (if (contains? @seen id)
             coll
             (do
               (vswap! seen conj id)
               (rf coll el)))))))))

(defn join-params-ast
  [{::pc/keys [indexes]} ident]
  (let [{::pc/keys [index-oir index-resolvers]} indexes]
    {:type     :root
     :children (into []
                     (comp (map val)
                           cat
                           (map (partial get index-resolvers))
                           (map ::pc/params)
                           (map eql/query->ast)
                           (map :children)
                           cat
                           (distinct-by :dispatch-key))
                     (get index-oir ident))}))

(defn content->form-query
  [{::keys [mutation] :as env}]
  (-> {:type     :root
       :children (-> (pc/mutation-data env mutation)
                     ::pc/params
                     eql/query->ast
                     :children
                     (conj {:type         :prop
                            :dispatch-key ::mutation-prefix
                            :key          ::mutation-prefix}))}
      eql/ast->query))

(defn data->form
  [{::keys [mutation] :as env}
   {::keys [mutation-prefix]
    :as    data}]
  (let [{::pc/keys [params]} (pc/mutation-data env mutation)]
    {::action (str mutation-prefix "/" mutation)
     ::label  (pr-str mutation)
     ::inputs (for [{:keys [dispatch-key params]} (:children (eql/query->ast params))]
                {::value (get data dispatch-key)
                 ::label (pr-str dispatch-key)
                 ::name  (str (namespace dispatch-key)
                              "/" (name dispatch-key))})}))

(defn display-properties-ast
  [{::keys [display-properties]
    :as    env}]
  {:type     :root
   :children (into []
                   (comp
                     (map (fn [prop]
                            (if-let [{::pc/keys [params]} (pc/mutation-data env prop)]
                              (:children (eql/query->ast params))
                              [{:type         :prop
                                :key          prop
                                :dispatch-key prop}])))
                     cat
                     (distinct-by :dispatch-key))
                   display-properties)})

(defn content->table-query
  [{::keys [default-params join-key] :as env}]
  (let [join-node (assoc (display-properties-ast env)
                    :type :join
                    :dispatch-key join-key
                    :key join-key
                    :params (assoc default-params
                              :pathom/as ::edges))]
    (-> (join-params-ast env join-key)
        (update :children conj
                {:type         :prop
                 :dispatch-key ::mutation-prefix
                 :key          ::mutation-prefix}
                join-node)
        eql/ast->query)))

(defn data->table
  [{::keys [join-key params-as default-params ident-label display-properties] :as env}
   {::keys [mutation-prefix edges]
    :as    el}]
  (let [->label (fn [ident]
                  (or (get ident-label ident)
                      (pr-str ident)))
        dispatch-as (into {}
                          (map (juxt val key))
                          params-as)]
    {::forms         (remove
                       (comp empty? ::inputs)
                       [{::inputs (for [{:keys [dispatch-key]} (:children (join-params-ast env join-key))
                                        :let [ident (get dispatch-as dispatch-key dispatch-key)]]
                                    {::value (second (or (find el dispatch-key)
                                                         (find default-params dispatch-key)))
                                     ::label (->label dispatch-key)
                                     ::name  (if (qualified-ident? ident)
                                               (str (namespace ident)
                                                    "/" (name ident))
                                               (name ident))})}])
     ::vs-table-body {::head (for [prop display-properties]
                               {::label (->label prop)})
                      ::rows (for [el edges]
                               {::columns (for [prop display-properties
                                                :let [{::pc/keys [params]
                                                       :as       mutation} (pc/mutation-data env prop)
                                                      value (get el prop)]]
                                            (cond-> {::value (if (keyword? value)
                                                               (->label value)
                                                               value)}
                                                    mutation (assoc ::forms [{::action (str mutation-prefix "/" prop)
                                                                              ::label  (str prop)
                                                                              ::inputs (for [param params]
                                                                                         {::value   (get el param)
                                                                                          ::name    (str (namespace param)
                                                                                                         "/" (name param))
                                                                                          ::hidden? true})}])))})}}))
