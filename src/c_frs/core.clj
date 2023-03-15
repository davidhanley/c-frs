(ns c-frs.core
  (:gen-class))

(require '[clj-time.coerce :as c]
         '[clojure.string :as str]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(def fractions (map #(/ 5 (+ 5 %)) (range)))

(defn get-scores-list-base [base]
  (map #(* % base) fractions))

(def get-scores-list (memoize get-scores-list-base))

(defn get-gender-from-string [gs]
  ({\M :male, \F :female} (first (remove #(= % \*) gs))))

(defn safe-parse-int [str]
  (try
    (Integer/parseInt str)
    (catch Exception e nil)))

(defn athlete-from-row [row]
  (let [[_ name age-str gender-str] row
        parsed-gender (get-gender-from-string gender-str)]
    (if parsed-gender [{:athlete-name name :gender parsed-gender :age (safe-parse-int age-str)}] nil)))

(def parse-date c/to-long)
(defn get-race-from-strings [sheet-strings date-filter]
  (let [[namestr datestr _ pointsstr & rest] sheet-strings
        date (parse-date (first datestr))
        points (safe-parse-int (first pointsstr))
        header {:race-name (first namestr), :date date :race-points points}]
    (if (and points date (date-filter date))
      (doall (map #(conj header %) (mapcat athlete-from-row rest)))
      [])))


(defn clean-line [line] (map #(str/trim (str/upper-case %)) line))
(defn read-file-into [filename pfunc]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (pfunc (map clean-line (csv/read-csv rdr))))))

(defn scan-directories []
  (->>
    (clojure.java.io/file "TowerRunningRaceData")
    (file-seq)
    (map str)
    (filter (fn [x] (clojure.string/ends-with? x ".csv")))))

(defn read-race [fn filter]
  (read-file-into fn #(get-race-from-strings % filter)))

(defn athlete-comp [a1 a2]
  (let [nc (compare (:athlete-name a1) (:athlete-name a2))]
    (if (not (zero? nc)) nc
                         (compare (:age a1) (:age a2)))))

(defn sort-athletes [athletes]
  (sort athlete-comp athletes))

(defn main-loop []
  (->>
    (scan-directories)
    (pmap #(read-race % (fn [_] true)))
    (apply concat)
    (sort-athletes)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Athlete count:" (count (main-loop)))
  (shutdown-agents)
  )
