(ns c-frs.core
  (:gen-class))

(require '[clj-time.coerce :as c]
         '[clojure.string :as str]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[clj-time.core :as t])

(def fractions
  "an infinite series of fractions, of the form 5/5 5/6 5/7 5/8 ..."
  (map #(/ 5 (+ 5 %)) (range)))

(defn get-scores-list-base
  "uses the fractions above to make an infinite points list, multiplying the points by the fractions"
  [base]
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
    (some-> str clojure.string/trim Long/parseLong)
    (catch NumberFormatException _ nil)))

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
        scores (get-scores-list points)
        header (conj (parse-name-and-category (first namestr)) {:date date :race-points points})
        athletes (map athlete-from-row rest)
        {:keys [male female]} (group-by :sex athletes)
        add-scores-and-rank (fn [a] (map #(conj header %1 {:points-scored %2 :overall-rank (inc %3)}) a scores (range)))
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

(defn ages-compatible? [athlete1 athlete2]
  (let [age1 (:age athlete1) age2 (:age athlete2)]
    (cond
      (nil? age1) true
      (nil? age2) true
      :else (<= (abs (- age1 age2)) 1))))

(defn partition-athlete [ath-list]
  (partition-when ages-compatible? (sort-by :age ath-list)))

(defn create-athlete-row [races]
  "create the result row struct, age, total points, name, event, etc"
  (merge (select-keys (first races) [:name :sex :foreign])
         {:total  (reduce + (map :points-scored (take 5 races)))
          :age    (some->> (keep :age races) not-empty (apply max))
          :events races}))

(defn deduplicate-by-race-max-points
  "Returns a vector of unique race results (one per :race-name with the highest :points),
   sorted by points in descending order (best performance first)."
  [results]
  (->> results
       (group-by :race-name)
       (map (comp (partial apply max-key :points-scored) val))
       (sort-by :points-scored >)
       vec))

(defn process-lines
  "Applies f to each non-blank, non-comment line.
   f receives the trimmed line as string."
  [filepath f]
  (with-open [rdr (io/reader filepath)]
    (->> (line-seq rdr)
         (remove #(or (str/blank? %) (str/starts-with? % "#")))
         (map f)
         doall)))                                           ; force realization before closing file

(defn name-translator-factory []
  (let [rules (process-lines "TowerRunningRaceData/translate.dat"
                             (fn [line]
                               (let [[pattern name] (map str/trim (str/split line #"," 2))]
                                 [(re-pattern pattern) name])))]
    (fn [athlete]
      (let [name (:name athlete)]
        (or (some (fn [[re repl]]
                    (when (re-matches re name)
                      (assoc athlete :name repl)))
                  rules)
            athlete)))))

(def one-year-ago
  (t/minus (t/now) (t/years 1)))

(defn recent-enough? [race-date]
  (t/after? (c/from-long race-date) one-year-ago))

(defn foreign-marker-factory []
  "Reads TowerRunningRaceData/foreign.dat and returns a function that
   adds :foreign true to athletes whose :name exactly matches a line in the file."
  (let [foreign-names (set (process-lines "TowerRunningRaceData/foreign.dat"
                                          (fn [line] (str/upper-case (str/trim line)))))]
    (fn [athlete]
      (if (contains? foreign-names (:name athlete))
        (assoc athlete :foreign true)
        athlete))))

(defn compute-overall-result-sheet []
  (->>
    (scan-directories)
    (map #(read-race % recent-enough?))
    (apply concat)
    (map (comp (foreign-marker-factory) (name-translator-factory)))
    (group-by :name)
    (mapcat rest)
    (mapcat partition-athlete)
    (map deduplicate-by-race-max-points)
    (map create-athlete-row)
    (vec)))

(defn format-points [points]
  (format "%.2f" (double points)))

(defn name-and-category
  "Returns 'Race Name' or 'Race Name - Category' if category exists."
  [entry]
  (if-let [category (:category entry)]
    (str (:race-name entry) " - <br>" category)
    (:race-name entry)))


(def c1 "#d8dbff")
(def c2 "#fdf4f8")
(defn add-row-ranks [sorted-athletes]
  (->>
    sorted-athletes
    (map (fn [a i] (assoc i :index a)) (rest (range)))
    (reduce
      (fn [acc ath]
        (let [prev (first acc)
              same-points (= (:total ath) (:total prev))]
        (cons
            (if same-points
              (assoc ath :tie? true :index (:index prev) :color (or (:color prev) :blue))
              (assoc ath :color ( {c1 c2 c2 c1 nil c1} (:color prev)))) acc))) nil)
    (reverse)))

(defn print-to-file
  "Writes athletes to an HTML file as a simple table.
   One row per athlete: name | age | total points | event1 | event2 | event3 | ...
   Filename format: sex-ageRange-foreign.txt → but saved as .html

   Assumes:
   - athletes is already filtered and sorted
   - :events list inside each athlete is already in desired display order"
  [athletes sex age-range foreign?]
  (let [sex-str (case sex :male "male" :female "female" (name sex))

        age-str (if (vector? age-range)
                  (str (first age-range) "-" (second age-range))
                  "all")

        foreign-str (if foreign? "all" "us-only")

        ;; filename without extension
        base-name (str/join "-" [sex-str age-str foreign-str])
        filename (str "content/" base-name ".html")

        title (str (str/capitalize sex-str) " — "
                   (if age-range
                     (str "Ages " (first age-range) "–" (second age-range))
                     "All Ages")
                   " — " (if foreign? "All Countries" "US Only"))]

    (with-open [w (io/writer filename)]
      (let [write (fn [& tokens]
                    (.write w (clojure.string/join (map str tokens))) )]
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
        (write "  <p>Generated: " (java.time.LocalDateTime/now) " — " (count athletes) " athletes</p>")

        (write "  <table>")
        (write "    <thead>\n      <tr>")
        (write "        <th>Name</th>")
        (write "        <th>Age</th>")
        (write "        <th>Total Points</th>")

        ;; We don't know max events in advance → we just write "Event 1", "Event 2", ...
        ;; You could compute max events first if you want fixed column headers
        (dotimes [i 10]                                     ; assume max ~10 events is reasonable
          (write "        <th>Event " (inc i) "</th>"))
        (write "      </tr>\n    </thead>")
        (write "    <tbody>")

        ;; One row per athlete
        (doseq [athlete (add-row-ranks athletes)]
          (let [athlete-name (:name athlete)
                age (or (:age athlete) "N/A")
                total (format-points (:total athlete))
                events (:events athlete)
                color (name (:color athlete))
                event-cells (map (fn [ev]
                                   (str (name-and-category ev)
                                        " <hr> Points: " (format-points (:points-scored ev)) " <br>rank " (:overall-rank ev)))
                                 events)]
            (write "      <tr style=\"background-color:" color ";\">")
            (write "        <td>" (str/escape (str (:index athlete) ". " athlete-name) {\& "&amp;" \< "&lt;" \> "&gt;"}) "</td>")
            (write "        <td>" age "</td>")
            (write "        <td class=\"points\">" total "</td>")

            ;; Event columns — pad with empty cells if fewer than 10 events
            (doseq [cell (concat event-cells (repeat 10 ""))]
              (write "        <td>" (or cell "") "</td>"))
            (write "      </tr>")))

        (write "    </tbody>\n  </table>")
        (write "</body>\n</html>"))

      (println "Wrote HTML table to" filename "—" (count athletes) "rows"))))

(defn print-both-to-file [athletes sex age-range]
  (print-to-file athletes sex age-range true)
  (print-to-file (remove :foreign athletes) sex age-range false))

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
        (print-both-to-file sorted sex nil)
        (doseq [range age-ranges]
          (print-both-to-file (filter-ages sorted range) sex range))))))






