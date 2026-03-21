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

(defn get-sex-from-string [gs]
  (some-> (str/replace gs "*" "")
          first
          Character/toUpperCase
          {\M :male \F :female}))

(defn safe-parse-int [str]
  (try
    (Integer/parseInt str)
    (catch Exception e nil)))

(defn athlete-from-row [row]
  (let [[_ name age-str sex-str] row]
    {:name name
     :sex  (get-sex-from-string sex-str)
     :age  (safe-parse-int age-str)}))

(def parse-date c/to-long)
(defn get-race-from-strings [sheet-strings date-filter]
  (let [[namestr datestr _ pointsstr & rest] sheet-strings
        date (parse-date (first datestr))
        points (safe-parse-int (first pointsstr))
        scores (get-scores-list-base points)
        header {:race-name (first namestr), :date date :race-points points}
        athletes (map athlete-from-row rest)
        [g1 g2] (partition-by :sex athletes)
        add-scores-and-rank (fn [a] (map #(conj header %1 {:points %2 :overall-rank (+ 1 %3)}) a scores (range)))
        ]
    (if (and points date (date-filter date))
      (mapcat add-scores-and-rank [g1 g2])
      )))

(def trim-and-upper (comp str/trim str/upper-case))
(defn clean-line [line] (map trim-and-upper line))

(defn read-file-into [filename pfunc]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (pfunc (map clean-line (csv/read-csv rdr))))))

(defn scan-directories []
  (->>
    (clojure.java.io/file "TowerRunningRaceData")
    (file-seq)
    (map str)
    (filter (fn [filename] (str/ends-with? filename ".csv")))))

(defn read-race [fn filter]
  (read-file-into fn #(get-race-from-strings % filter)))

(defn partition-when
  "Like partition-by but decides split based on consecutive pairs"
  [should-continue? coll]
  (reduce
    (fn [acc x]
      (if (or (empty? acc)
              (should-continue? (last (last acc)) x))
        (update acc (dec (count acc)) conj x)
        (conj acc [x])))
    [[(first coll)]]
    (next coll)))
(defn ages-compatible? [a b]
  (let [aa (:age a) ab (:age b)]
    (cond
      (nil? aa) true
      (nil? ab) true
      :else     (<= (abs (- aa ab)) 1))))

(defn partition-athlete [ath-list]
  (partition-when ages-compatible? ath-list))

(defn main-loop []
  (->>
    (scan-directories)
    (pmap #(read-race % (fn [_] true)))
    (apply concat)
    (group-by :name)
    (mapcat partition-athlete)
    ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "hi, dave!")
  (doseq [ath (main-loop)]
    (println ath))
  (shutdown-agents))





