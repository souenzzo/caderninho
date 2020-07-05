(ns ron.reader
  (:refer-clojure :exclude [read-string])
  (:require [clojure.edn :as edn]))

(defn do-next
  [{::keys [next-chars]
    :as    env}]
  (assoc env
    ::dispatch-char (first next-chars)
    ::next-chars (rest next-chars)))


(declare read-impl)

(defn string-reader-impl
  [{::keys [next-chars]
    :as    env}]
  (let [[xs [_ dispatch-char & next-chars]] (split-with (complement #{\"})
                                                        next-chars)]
    (assoc env
      ::next-chars next-chars
      ::dispatch-char dispatch-char
      ::value (apply str xs))))

(defn numeric?
  [x]
  (contains? (set "0123456789-.") x))

(defn number-reader-impl
  [{::keys [dispatch-char next-chars]
    :as    env}]
  (let [[ns next-chars] (split-with numeric?
                                    next-chars)]
    (assoc env
      ::next-chars (rest next-chars)
      ::dispatch-char (first next-chars)
      ::value (edn/read-string (apply str dispatch-char ns)))))


(defn true-reader-impl
  [{::keys [next-chars]
    :as    env}]
  (let [[#_t _r _u _e dispatch-char & next-chars] next-chars]
    (assoc env
      ::next-chars next-chars
      ::dispatch-char dispatch-char
      ::value true)))

(defn false-reader-impl
  [{::keys [next-chars]
    :as    env}]
  (let [[#_f _a _l _s _e dispatch-char & next-chars] next-chars]
    (assoc env
      ::next-chars next-chars
      ::dispatch-char dispatch-char
      ::value false)))

(defn null-reader-impl
  [{::keys [next-chars]
    :as    env}]
  (let [[#_n _u _l _l dispatch-char & next-chars] next-chars]
    (assoc env
      ::next-chars next-chars
      ::dispatch-char dispatch-char
      ::value nil)))

(defn whitespace?
  [x]
  (contains? #{\tab
               \newline
               \return
               \space}
             x))

(defn array-reader-impl
  [env]
  (loop [{::keys [array dispatch-char]
          :as    env} (assoc (do-next env)
                        ::array (transient []))]
    (cond
      (nil? dispatch-char) (throw (ex-info "eof" env))
      (whitespace? dispatch-char) (recur (do-next env))
      (contains? #{\,} dispatch-char) (recur (do-next env))
      (contains? #{\]} dispatch-char) (assoc (do-next env)
                                        ::value (persistent! array))
      :else (let [{::keys [value]
                   :as    env} (read-impl env)]
              (recur (assoc env
                       ::array (conj! array value)))))))

(defn double-dot-reader
  [{::keys [dispatch-char]
    :as    env}]
  (cond
    (whitespace? dispatch-char) (recur (do-next env))
    (= \: dispatch-char) (do-next env)
    :else (throw (ex-info "expected ':'" env))))

(defn object-reader-impl
  [env]
  (loop [{::keys [object dispatch-char]
          :as    env} (assoc (do-next env)
                        ::object (transient {}))]
    (cond
      (nil? dispatch-char) (throw (ex-info "eof" env))
      (whitespace? dispatch-char) (recur (do-next env))
      (contains? #{\,} dispatch-char) (recur (do-next env))
      (contains? #{\}} dispatch-char) (assoc (do-next env)
                                        ::value (persistent! object))
      :else (let [{key ::value
                   :as env} (read-impl env)
                  {value ::value
                   :as   env} (read-impl (double-dot-reader env))]
              (recur (assoc env
                       ::object (assoc! object key value)))))))

(defn read-impl
  [{::keys [dispatch-char]
    :as    env}]
  (cond
    (whitespace? dispatch-char) (read-impl (do-next env))
    (contains? #{\"} dispatch-char) (string-reader-impl env)
    (contains? #{\t} dispatch-char) (true-reader-impl env)
    (contains? #{\f} dispatch-char) (false-reader-impl env)
    (numeric? dispatch-char) (number-reader-impl env)
    (contains? #{\n} dispatch-char) (null-reader-impl env)
    (contains? #{\[} dispatch-char) (array-reader-impl env)
    (contains? #{\{} dispatch-char) (object-reader-impl env)))

(defn read-string
  [s]
  (::value (read-impl {::dispatch-char (first s)
                       ::next-chars    (rest s)})))