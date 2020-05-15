(ns net.molequedeideias.inga
  (:require [edn-query-language.core :as eql]
            [com.wsscode.pathom.connect :as pc]
            [clojure.spec.alpha :as s]))

(def show (letfn [(show [x]
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
                             (pr-str x)]))]
            show))


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

(defn ident-params-ast
  [indexes ident]
  (let [{::pc/keys [index-oir index-resolvers]} indexes]
    {:type     :root
     :children (sequence
                 (comp (map val)
                       cat
                       (map (partial get index-resolvers))
                       (map ::pc/params)
                       (map eql/query->ast)
                       (map :children)
                       cat
                       (distinct-by :dispatch-key))
                 (get index-oir ident))}))



(defn content->table-query
  [{::pc/keys [indexes]
    ::keys    [ident-key display-properties
               mutation-prefix-ident
               default-params join-key]}]
  (let [{::pc/keys [index-mutations]} indexes
        eid-key (if (contains? default-params ident-key)
                  (find default-params ident-key)
                  ident-key)
        join-params (when default-params
                      (let [after (dissoc default-params ident-key)]
                        (if (empty? after)
                          nil
                          after)))
        join-node {:type         :join
                   :dispatch-key join-key
                   :key          join-key
                   :children     (concat (->> (ident-params-ast indexes join-key)
                                              :children
                                              (remove (comp #{:edn-query-language.pagination/edges}
                                                            :dispatch-key)))
                                         [{:type         :join
                                           :key          :edn-query-language.pagination/edges
                                           :dispatch-key :edn-query-language.pagination/edges
                                           :children     (sequence
                                                           (comp
                                                             (map (fn [{:keys [dispatch-key] :as node}]
                                                                    (if-let [{::pc/keys [params]} (get index-mutations dispatch-key)]
                                                                      (:children (eql/query->ast params))
                                                                      [node])))
                                                             cat
                                                             (distinct-by :dispatch-key))
                                                           (:children (eql/query->ast display-properties)))}])}]
    (-> {:type     :root
         :children (remove nil? [(when mutation-prefix-ident
                                   {:type         :prop
                                    :dispatch-key mutation-prefix-ident
                                    :key          mutation-prefix-ident})
                                 {:type         :join
                                  :dispatch-key ident-key
                                  :key          eid-key
                                  :children     [(cond-> join-node
                                                         join-params (assoc :params join-params))]}
                                 {:type         :prop
                                  :dispatch-key ::pc/indexes
                                  :key          ::pc/indexes}])}
        eql/ast->query)))

(s/fdef content->table-query
        :args (s/cat :env (s/keys :req [::ident-key
                                        ::display-properties
                                        ::default-params
                                        ::join-key
                                        ::pc/indexes])))

(defn content->form-query
  [{::pc/keys [indexes]
    ::keys    [mutation-prefix-ident mutation]}]
  (let [params (-> (pc/mutation-data indexes mutation)
                   ::pc/params
                   eql/query->ast
                   :children)]
    (-> {:type     :root
         :children (concat [{:type         :prop
                             :dispatch-key ::pc/indexes
                             :key          ::pc/indexes}
                            {:type         :prop
                             :dispatch-key mutation-prefix-ident
                             :key          mutation-prefix-ident}]
                           params)}
        eql/ast->query)))

(s/fdef content->form-query
        :args (s/cat :env (s/keys)))

(defn data->form
  [{::keys [mutation mutation-prefix-ident]
    :as    env}
   {::pc/keys [indexes]
    :as       data}]
  (let [{::pc/keys [index-mutations]} indexes
        mutation-prefix (get data mutation-prefix-ident)
        {::pc/keys [params]} (get index-mutations mutation)]
    {::action (str mutation-prefix "/" mutation)
     ::label  (pr-str mutation)
     ::inputs (concat (for [{:keys [dispatch-key params]} (:children (eql/query->ast params))]
                        {::value (get data dispatch-key)
                         ::label (pr-str dispatch-key)
                         ::name  (str (namespace dispatch-key)
                                      "/" (name dispatch-key))}))}))

(s/fdef data->form
        :args (s/cat :env (s/keys :req [::mutation
                                        ::mutation-prefix])
                     :data (s/keys :req [::pc/indexes])))

(defn data->table
  [{::keys [mutation-prefix-ident
            ident-key
            join-key
            params-as
            default-params
            ident-label
            display-properties]}
   {::pc/keys [indexes]
    :as       data}]
  (let [->label (fn [ident]
                  (or (get ident-label ident)
                      (pr-str ident)))
        mutation-prefix (get data mutation-prefix-ident)
        dispatch-as (into {}
                          (map (juxt val key))
                          params-as)
        {::pc/keys [index-mutations]} indexes
        {:edn-query-language.pagination/keys [edges] :as el} (get-in data [(if (contains? default-params ident-key)
                                                                             (find default-params ident-key)
                                                                             ident-key)
                                                                           join-key])
        {:keys [children]} (eql/query->ast display-properties)]
    {::forms         (remove
                       (comp empty? ::inputs)
                       [{::inputs (for [{:keys [dispatch-key]} (:children (ident-params-ast indexes join-key))
                                        :let [ident (get dispatch-as dispatch-key dispatch-key)]]
                                    {::value (second (or (find el dispatch-key)
                                                         (find default-params dispatch-key)))
                                     ::label (->label dispatch-key)
                                     ::name  (if (qualified-ident? ident)
                                               (str (namespace ident)
                                                    "/" (name ident))
                                               (name ident))})}])
     ::vs-table-body {::head (for [{:keys [dispatch-key]} children]
                               {::label (->label dispatch-key)})
                      ::rows (doall (for [el edges]
                                      {::columns (for [{:keys [dispatch-key]} children
                                                       :let [{::pc/keys [params]
                                                              :as       mutation} (get index-mutations dispatch-key)
                                                             value (get el dispatch-key)]]
                                                   (cond-> {::value (if (keyword? value)
                                                                      (->label value)
                                                                      value)}
                                                           mutation (assoc ::forms [{::action (str mutation-prefix "/" dispatch-key)
                                                                                     ::inputs (for [param params]
                                                                                                {::value   (get el param)
                                                                                                 ::name    (str (namespace param)
                                                                                                                "/" (name param))
                                                                                                 ::hidden? true})}])))}))}}))

(s/fdef data->table
        :args (s/cat :env (s/keys :req [::mutation-prefix
                                        ::ident-key
                                        ::default-params
                                        ::join-key
                                        ::display-properties])
                     :data (s/keys :req [::pc/indexes])))

