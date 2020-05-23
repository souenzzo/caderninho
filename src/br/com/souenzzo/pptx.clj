(ns br.com.souenzzo.pptx
  (:require [clojure.java.io :as io])
  (:import (org.apache.poi.xslf.usermodel XMLSlideShow)))

(defn slide-show
  [f]
  (XMLSlideShow. (io/input-stream f)))
