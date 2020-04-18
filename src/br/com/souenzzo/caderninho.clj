(ns br.com.souenzzo.caderninho)

(defn -main
  [& opts]
  (prn {:opts opts :env (System/getenv) :props (System/getProperties)}))
