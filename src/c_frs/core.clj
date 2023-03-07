(ns c-frs.core
  (:gen-class))

(def fractions (map (fn [x] (/ 5 (+ 5 x))) (range)))

(defn get-scores-list-base [base]
  (map (fn [x] (* x base)) fractions))

(def get-scores-list (memoize get-scores-list-base))

(defn read-file-into [filename pfunc]
  (with-open [rdr (clojure.java.io/reader filename)]
    (pfunc (line-seq rdr))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
