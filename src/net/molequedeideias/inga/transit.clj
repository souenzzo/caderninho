(ns net.molequedeideias.inga.transit
  (:refer-clojure :exclude [pr pr-str read read-string])
  (:require [clojure.java.io :as io]
            [cognitect.transit :as t])
  (:import (java.io ByteArrayOutputStream)))

(defn pr
  ([out o]
   (pr out o {}))
  ([out o {:keys [type]
           :or   {type :json}
           :as   opts}]
   (t/write (t/writer out type opts)
            o)))

(defn pr-str
  ([x] (pr-str x {}))
  ([x opts]
   (-> (ByteArrayOutputStream.)
       (doto (pr x opts))
       str)))

(defn read
  ([in] (read in {}))
  ([in {:keys [type]
        :or   {type :json}
        :as   opts}]
   (-> (t/reader in type opts)
       (t/read))))

(defn read-string
  ([s] (read-string s {}))
  ([s opts]
   (read (io/input-stream (.getBytes s))
         opts)))
