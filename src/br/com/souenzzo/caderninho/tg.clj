(ns br.com.souenzzo.caderninho.tg
  (:require [clojure.data.json :as json]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.diplomat.http :as p.http]
            [com.wsscode.pathom.diplomat.http.clj-http :as clj-http]
            [io.pedestal.http :as http]
            [hiccup2.core :as h]
            [clojure.tools.reader.edn :as edn]
            [clojure.string :as string])
  (:import (java.util Date)
           (java.time Instant)
           (java.net URLDecoder URLEncoder)
           (java.nio.charset StandardCharsets)))

(def telegram-bot-token
  (System/getenv "TELEGRAM_BOT_TOKEN"))

(def register
  [(pc/resolver `get-me
                {::pc/output [:telegram.api/me]}
                (fn [env _]
                  (let [url (str "https://api.telegram.org/bot" telegram-bot-token "/getMe")
                        {:keys [is_bot first_name username can_join_groups can_read_all_group_messages supports_inline_queries
                                id]} (-> (assoc env ::p.http/url url
                                                    ::p.http/method ::p.http/get
                                                    ::p.http/as ::http/json)
                                         p.http/request
                                         ::p.http/body
                                         :result)]
                    {:telegram.api/me {:telegram.user/id                           id
                                       :telegram.user/bot?                         is_bot
                                       :telegram.user/first-name                   first_name
                                       :telegram.user/username                     username
                                       :telegram.user/can-join-groups?             can_join_groups
                                       :telegram.user/can-read-all-group-messages? can_read_all_group_messages
                                       :telegram.user/supports-inline-queries?     supports_inline_queries}})))
   (pc/resolver `get-webhook
                {::pc/output [:telegram.webhook-info/url
                              :telegram.webhook-info/custom-certificate?
                              :telegram.webhook-info/pending-update-count]}
                (fn [env _]
                  (let [url (str "https://api.telegram.org/bot" telegram-bot-token "/getWebhookInfo")
                        {:keys [url has_custom_certificate pending_update_count]} (-> (assoc env ::p.http/url url
                                                                                                 ::p.http/method ::p.http/get
                                                                                                 ::p.http/as ::http/json)
                                                                                      p.http/request
                                                                                      ::p.http/body
                                                                                      (doto prn)
                                                                                      :result)]
                    {:telegram.webhook-info/url                  url
                     :telegram.webhook-info/custom-certificate?  has_custom_certificate
                     :telegram.webhook-info/pending-update-count pending_update_count})))
   (pc/mutation `telegram.api/set-webhook
                {::pc/parms [:telegram.webhook-info/url]}
                (fn [env {:telegram.webhook-info/keys [url]}]
                  (let [url (str "https://api.telegram.org/bot" telegram-bot-token
                                 "/setWebhook?url=" (URLEncoder/encode
                                                      (str url)
                                                      (str StandardCharsets/UTF_8)))
                        body (-> (assoc env ::p.http/url url
                                            ::p.http/method ::p.http/get
                                            ::p.http/as ::http/json)
                                 p.http/request
                                 ::p.http/body)]
                    (prn body)
                    {})))
   (pc/resolver `get-chat
                {::pc/input  #{:telegram.chat/id}
                 ::pc/output [:telegram.chat/type]}
                (fn [env {:telegram.chat/keys [id]}]
                  (let [url (str "https://api.telegram.org/bot" telegram-bot-token "/getChat?chat_id=" id)
                        {:keys [id first_name username type photo]} (-> (assoc env ::p.http/url url
                                                                                   ::p.http/method ::p.http/get
                                                                                   ::p.http/as ::p.http/json)
                                                                        p.http/request
                                                                        ::p.http/body
                                                                        :result)]
                    {:telegram.chat/type (keyword "telegram.chat.type"
                                                  type)})))
   (pc/resolver `get-updates
                {::pc/output [:telegram.api/updates]}
                (fn [env _]
                  (let [url (str "https://api.telegram.org/bot" telegram-bot-token "/getUpdates")
                        updates (-> (assoc env ::p.http/url url
                                               ::p.http/method ::p.http/get
                                               ::p.http/as ::p.http/json)
                                    p.http/request
                                    ::p.http/body
                                    :result)]
                    {:telegram.api/updates (for [{:keys [update_id message] :as update} updates
                                                 :let [{:keys [message_id entities from chat date text]} message]]
                                             {:telegram.update/id        update_id
                                              :telegram.update/raw       update
                                              :telegram.message/date     (Date/from (Instant/ofEpochSecond date))
                                              :telegram.message/text     text
                                              :telegram.chat/id          (-> chat :id)
                                              :telegram.message/entities (for [{:keys [type offset length]} entities]
                                                                           {:telegram.message.entity/type   (keyword "telegram.message.entity"
                                                                                                                     type)
                                                                            :telegram.message.entity/length length
                                                                            :telegram.message/text          text
                                                                            :telegram.message.entity/offset offset})
                                              :telegram.message/id       message_id})})))])

(def parser
  (->> register
       (hash-map ::pc/register)
       pc/connect-plugin
       vector
       (hash-map
         ::p/mutate pc/mutate
         ::p/env {::p/reader               [p/map-reader
                                            pc/reader3
                                            pc/open-ident-reader
                                            p/env-placeholder-reader]
                  ::p.http/driver          clj-http/request
                  ::p/placeholder-prefixes #{">"}}
         ::p/plugins)
       (p/parser)))

(defn about
  [req]
  (let [data (parser req [:telegram.webhook-info/url
                          :telegram.webhook-info/custom-certificate?
                          :telegram.webhook-info/pending-update-count])
        body [:div
              (for [[k v] data]
                [:pre (pr-str k v)])
              [:div
               [:form
                {:action "/set-webhook" :method "POST"}
                [:input {:name "url"}]
                [:input {:type "submit"}]]]]]
    {:headers {"Content-Type" "text/html"}
     :body    (str (h/html
                     (h/raw "<!DOCTYPE html>")
                     [:head
                      [:meta {:chatset "utf-8"}]
                      [:title "about caderninho"]]
                     [:body
                      body]))
     :status  200}))

(defn set-webhook
  [{:keys [body] :as req}]
  (let [{:keys [url]} (into {}
                            (comp (map (fn [s]
                                         (string/split s #"=" 2)))
                                  (map (fn [[^String k ^String v]]
                                         [(keyword (URLDecoder/decode k (str StandardCharsets/UTF_8)))
                                          (URLDecoder/decode v (str StandardCharsets/UTF_8))])))

                            (string/split (slurp body)
                                          #"&"))]
    (try
      (parser req `[(telegram.api/set-webhook ~{:telegram.webhook-info/url url})])
      (catch Throwable ex
        (def __ex ex)
        (println ex)))
    {:headers {"Location" "/about"}
     :status  301}))

(def service-map
  {::http/routes `#{["/about" :get about]
                    ["/set-webhook" :post set-webhook]}
   ::http/join?  false
   ::http/type   :jetty
   ::http/port   (edn/read-string (System/getenv "PORT"))})
(defonce http-state (atom nil))
(defn -main
  [& _]
  (let []
    (swap! http-state
           (fn [st]
             (when st
               (http/stop st))
             (-> service-map
                 http/default-interceptors
                 http/create-server
                 http/start)))))
