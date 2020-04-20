(ns net.molequedeideias.inga-bootstrap.ui
  (:require [net.molequedeideias.inga :as inga]))

(def Input
  {::inga/query [::inga/label ::inga/name ::inga/type ::inga/value ::inga/hidden? ::inga/flex-end?]
   ::inga/ui    (fn [{::inga/keys [label name type value hidden? flex-end? possible-values multiple?]}]
                  [:label
                   (cond-> {:style (cond-> {}
                                           flex-end? (assoc :align-self "flex-end"))}
                           hidden? (assoc :hidden true))
                   label
                   (if multiple?
                     [:select {:class    "form-control"
                               :multiple multiple?
                               :name     name}
                      (for [{::inga/keys [value selected?]} possible-values]
                        [:option {:selected selected?
                                  :value    value} value])]
                     [:input
                      (cond-> {:class "form-control"
                               :name  name}
                              type (assoc :type type)
                              (= type "submit") (assoc :class "btn btn-primary")
                              value (assoc :value value))])])})

(def ui-input (::inga/ui Input))

(def Form
  {::inga/query [::inga/action
                 {::inga/inputs (::inga/query Input)}]
   ::inga/ui    (fn [{::inga/keys [action inputs]}]
                  [:form
                   (cond-> {:style {:display        "flex"
                                    :padding        "3vw"
                                    :flex-direction "column"}}
                           action (assoc :method "POST"
                                         :enctype "multipart/form-data"
                                         :action action))
                   (map ui-input inputs)
                   (ui-input {::inga/flex-end? true
                              ::inga/type      "submit"})])})


(def ui-form (::inga/ui Form))

(def VSTableBody
  {::inga/query [{::inga/head [::inga/label]}
                 {::inga/rows [{::inga/columns [::inga/value]}
                               {::inga/forms (::inga/query Form)}]}]
   ::inga/ui    (fn [{::inga/keys [head rows]}]
                  (list
                    [:thead
                     [:tr
                      (for [{::inga/keys [label]} head]
                        [:th
                         label])]]
                    [:tbody
                     (for [{::inga/keys [columns]} rows]
                       [:tr
                        (for [{::inga/keys [value forms]} columns]
                          [:td
                           value
                           (map ui-form forms)])])]))})

(def ui-vs-table-body (::inga/ui VSTableBody))


(def Table
  {::inga/query [{:forms (::inga/query Form)}
                 {::inga/vs-table-body (::inga/query VSTableBody)}]
   ::inga/ui    (fn [{::inga/keys [vs-table-body forms]}]
                  (list
                    (map ui-form forms)
                    [:table
                     {:class "table table-striped"}
                     (ui-vs-table-body vs-table-body)]))})

(def ui-table (::inga/ui Table))
