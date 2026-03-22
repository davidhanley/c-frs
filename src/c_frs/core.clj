(ns c-frs.core
  (:gen-class))

(require '[clj-time.coerce :as c]
         '[clojure.string :as str]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[clj-time.core :as t])

(def fractions (map #(/ 5 (+ 5 %)) (range)))

(defn get-scores-list-base [base]
  (map #(* % base) fractions))

(def get-scores-list (memoize get-scores-list-base))

(defn get-sex-from-string [gs]
  (some-> gs
          (str/replace "*" "")
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

(defn parse-name-and-category [s]
  (zipmap [:race-name :category]
          (map str/trim (str/split (str/trim s) #"\s*-\s*" 2))))

(defn get-race-from-strings [sheet-strings date-filter]
  (let [[namestr datestr _ pointsstr & rest] sheet-strings
        date (parse-date (first datestr))
        points (safe-parse-int (first pointsstr))
        scores (get-scores-list-base points)
        header (conj (parse-name-and-category (first namestr)) {:date date :race-points points})
        athletes (map athlete-from-row rest)
        {:keys [male female]} (group-by :sex athletes)
        add-scores-and-rank (fn [a] (map #(conj header %1 {:points-scored %2 :overall-rank (+ 1 %3)}) a scores (range)))
        ]
    (if (and points date (date-filter date))
      (mapcat add-scores-and-rank [male female]))))

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
      :else (<= (abs (- aa ab)) 1))))

(defn partition-athlete [ath-list]
  (partition-when ages-compatible? (sort-by :age ath-list)))

(defn compute-points-to-use [races]
  {:total  (reduce + (map :points-scored (take 5 races)))
   :age    (some->> races
                    (keep :age)
                    not-empty
                    (apply max))
   :events races
   :name   (:name (first races))
   :sex    (:sex (first races))
   :foreign (:foreign (first races))
   })

(defn deduplicate-by-race-max-points
  "Returns a vector of unique race results (one per :race-name with the highest :points),
   sorted by points in descending order (best performance first)."
  [results]
  (->> results
       (group-by :race-name)
       (map (comp (partial apply max-key :points-scored) val))
       (sort-by :points-scored >)
       vec))


(defn name-translator-factory []
  (let [rules (with-open [rdr (io/reader "TowerRunningRaceData/translate.dat")]
                (->> (line-seq rdr)
                     (remove #(or (str/blank? %) (str/starts-with? % "#")))
                     (keep #(let [[p n] (str/split % #"," 2)]
                              (when (and p n)
                                [(re-pattern (str/trim p)) (str/trim n)])))
                     vec))]
    (fn [athlete]
      (if-let [name (:name athlete)]
        (or (some (fn [[re repl]]
                    (when (re-matches re name)
                      (assoc athlete :name repl)))
                  rules)
            athlete)
        athlete))))

(def one-year-ago
  (t/minus (t/now) (t/years 1)))

(defn recent-enough? [dt]
  (t/after? (c/from-long dt) one-year-ago))

(defn foreign-marker-factory
  "Reads TowerRunningRaceData/foreign.dat and returns a function that
   adds :foreign true to athletes whose :name exactly matches a line in the file."
  []
  (let [foreign-names
        (with-open [rdr (io/reader "TowerRunningRaceData/foreign.dat")]
          (->> (line-seq rdr)
               (remove #(or (str/blank? %) (str/starts-with? % "#")))
               (map str/trim)
               (into #{})))]

    (fn mark-foreign [athlete]
      (if-let [name (:name athlete)]
        (if (contains? foreign-names name)
          (assoc athlete :foreign true)
          athlete)
        athlete))))                                         ; no name → unchanged

(defn compute-overall-result-sheet []
  (->>
    (scan-directories)
    (map #(read-race % recent-enough?))
    (apply concat)
    (map (name-translator-factory))
    (map (foreign-marker-factory))
    (group-by :name)
    (mapcat rest)
    (mapcat partition-athlete)
    (map deduplicate-by-race-max-points)
    (map compute-points-to-use)
    (vec)                                                   ;; descending order: highest points first)
    ))

(defn format-points [points]
  (format "%.2f" (double points)))
(defn print-to-file
  "Writes athletes to an HTML file as a simple table.
   One row per athlete: name | age | total points | event1 | event2 | event3 | ...
   Filename format: sex-ageRange-foreign.txt → but saved as .html

   Assumes:
   - athletes is already filtered and sorted
   - :events list inside each athlete is already in desired display order"
  [athletes sex age-range foreign?]
  (let [sex-str (case sex :male "male" :female "female" (name sex))

        age-str (if (and age-range (vector? age-range))
                  (str (first age-range) "-" (second age-range))
                  "all")

        foreign-str (if foreign? "all" "us-only")

        ;; filename without extension
        base-name (str/join "-" [sex-str age-str foreign-str])
        filename (str base-name ".html")

        title (str (str/capitalize sex-str) " — "
                   (if age-range
                     (str "Ages " (first age-range) "–" (second age-range))
                     "All Ages")
                   " — " (if foreign? "All Countries" "US Only"))]

    (with-open [w (io/writer filename)]
      (let [write (fn [& strings]
                    (let [final-str (apply str strings)]
                      (.write w (str final-str ""))))]
        ;; Header + basic styling
        (write "<!DOCTYPE html>\n<html lang=\"en\">\n<head>")
        (write "  <meta charset=\"UTF-8\">")
        (write "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">")
        (write "  <title>" title "</title>")
        (write "  <style>")
        (write "    body { font-family: Arial, sans-serif; margin: 2em; }")
        (write "    h1 { text-align: center; }")
        (write "    table { border-collapse: collapse; width: 100%; max-width: 1400px; margin: 1em auto; }")
        (write "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }")
        (write "    th { background-color: #f2f2f2; }")
        (write "    tr:nth-child(even) { background-color: #f9f9f9; }")
        (write "    .points { text-align: right; }")
        (write "  </style>\n</head>\n<body>")

        (write "  <h1>" title "</h1>")
        (write "  <p>Generated: " (str (java.time.LocalDateTime/now)) " — " (count athletes) " athletes</p>")

        (write "  <table>")
        (write "    <thead>\n      <tr>")
        (write "        <th>Name</th>")
        (write "        <th>Age</th>")
        (write "        <th>Total Points</th>")

        ;; We don't know max events in advance → we just write "Event 1", "Event 2", ...
        ;; You could compute max events first if you want fixed column headers
        (dotimes [i 10]                                     ; assume max ~10 events is reasonable
          (write (str "        <th>Event " (inc i) "</th>")))
        (write "      </tr>\n    </thead>")
        (write "    <tbody>")

        ;; One row per athlete
        (doseq [[idx athlete] (map-indexed vector athletes)]
          (let [name (or (:name athlete) "—")
                age (or (:age athlete) "—")
                total (format-points (:total athlete))
                events (:events athlete)
                event-cells (map (fn [ev]
                                   (str (:race-name ev)
                                        " <hr> Points: " (format-points (:points-scored ev)) " <br>rank " (:overall-rank ev)))
                                 events)]
            (write "      <tr>")
            (write (str "        <td>" (str/escape (str (+ 1 idx) ". " name) {\& "&amp;" \< "&lt;" \> "&gt;"}) "</td>"))
            (write (str "        <td>" age "</td>"))
            (write (str "        <td class=\"points\">" total "</td>"))

            ;; Event columns — pad with empty cells if fewer than 10 events
            (doseq [cell (concat event-cells (repeat 10 ""))]
              (write (str "        <td>" (or cell "") "</td>")))
            (write "      </tr>")))

        (write "    </tbody>\n  </table>")
        (write "</body>\n</html>"))

      (println "Wrote HTML table to" filename "—" (count athletes) "rows"))))

(def age-ranges [[0 19] [20 29] [30 39] [40 49] [50 59] [60 69] [70 79] [80 89] [90 99] [100 200]])

(defn filter-ages [athletes [min-age max-age]]
  (filter (fn [athlete]
            (let [age (:age athlete)]
              (and age (<= age max-age) (>= age min-age))))
          athletes))


(defn -main
  [& args]
  (let [overall-results (compute-overall-result-sheet)
        grouped (group-by :sex overall-results)]
    (doseq [[sex athletes] grouped]
      (let [sorted (sort-by :total > athletes)]
        (print-to-file sorted sex nil true)
        (doseq [range age-ranges]
          (let [age-filtered (filter-ages sorted range)]
          (print-to-file age-filtered sex range true)
          (print-to-file (remove :foreign age-filtered) sex range false)
          ))
        ))))






