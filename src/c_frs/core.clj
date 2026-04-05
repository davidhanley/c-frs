(ns c-frs.core
  (:gen-class))

(require '[hiccup.core :as h]
         '[hiccup.page :as hp]
         '[clj-time.coerce :as c]
         '[clojure.string :as str]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[clojure.data.json :as json]
         '[clj-time.core :as t]
         '[clj-time.format :as f])

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

(def parse-date c/from-string)

(defn parse-name-and-category [s]
  (zipmap [:race-name :category]
          (map str/trim (str/split (str/trim s) #"\s*-\s*" 2))))

(defn to-url [fn]
  (str "https://github.com/davidhanley/TowerRunningRaceData/blob/main/" (last (str/split fn #"/"))))

(defn dedupe-athletes
  "Returns a sequence of athletes with duplicate names removed.
   Keeps the first occurrence of each name (preserves original order)."
  [athletes]
  (:result (reduce (fn [acc athlete]
                     (let [name (:name athlete)]
                       (if (contains? (:seen acc) name)
                         acc
                         (-> acc
                             (update :result conj athlete)
                             (update :seen conj name)))))
                   {:result [] :seen #{}}
                   athletes)))

(defn add-scores-and-rank [athletes header]
  (map #(assoc %1 :header header :points-scored %2 :overall-rank (inc %3)) athletes (get-scores-list (:race-points header)) (range)))

(defn dedupe-separate-and-score [header athletes]
  (let [{:keys [male female]} (group-by :sex (dedupe-athletes athletes))]
    (mapcat #(add-scores-and-rank % header) [male female])))

(defn get-race-from-csv-strings
  "Given a sequence of string, parse a race from that. The structure of a race is all the athletes who competed.
  As we operate on athletes, each athlete has a copy of the race info, points, date, name, etc."
  [sheet-strings date-filter filename]
  (let [[namestr datestr _ pointsstr & rest] sheet-strings
        date (parse-date (first datestr))
        points (safe-parse-int (first pointsstr))
        header (conj (parse-name-and-category (first namestr)) {:date date :race-points points :url (to-url filename)})
        ]
    (if (and points date (date-filter date))
      (dedupe-separate-and-score header (map athlete-from-row rest)))))

(def trim-and-upper (comp str/trim str/upper-case))
(defn clean-line [line] (map trim-and-upper line))

(defn read-csv-file-into
  "read a CSV into a sequence, clean the lines, and feed that to a suupplied function.
    Use doall to make sure all processing happens before the file is closed."
  [filename pfunc]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (pfunc (map clean-line (csv/read-csv rdr))))))

(defn scan-directories
  "Look for all the race data files, returning a list of them"
  []
  (->>
    (clojure.java.io/file "TowerRunningRaceData")
    (file-seq)
    (map str)
    (filter (fn [filename] (or (str/ends-with? filename ".csv") (str/ends-with? filename ".json"))))))

(defn read-csv-race [fn filter]
  "Map the race-to-strings function over the contents of the race file"
  (read-csv-file-into fn #(get-race-from-csv-strings % filter fn)))

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

(defn ages-compatible?
  "See if two athletes might be the same person based on age"
  [athlete1 athlete2]
  (let [age1 (:age athlete1)
        age2 (:age athlete2)]
    (or (nil? age1)
        (nil? age2)
        (<= (abs (- age1 age2)) 1))))

(defn partition-athlete
  "If there are two athletes with the same name, but there are very different ages, treat them as different athletes.
  this is very rare and expensive, so maybe this should test first and not make a new list each time"
  [ath-list]
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
       (group-by (fn [result] (:race-name (:header result))))
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
  (t/after? race-date one-year-ago))

(defn foreign-marker-factory []
  "Reads TowerRunningRaceData/foreign.dat and returns a function that
   adds :foreign true to athletes whose :name exactly matches a line in the file."
  (let [foreign-names (set (process-lines "TowerRunningRaceData/foreign.dat"
                                          (fn [line] (str/upper-case (str/trim line)))))]
    (fn [athlete]
      (if (contains? foreign-names (:name athlete))
        (assoc athlete :foreign true)
        athlete))))

(defn write-header
  "Write the HTML header for the pages.  Separate functon"
  [title]
  (hp/html5
    {:lang "en"}
    [:head
     [:meta {:charset "UTF-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:title title]
     [:style
      "body { font-family: Arial, sans-serif; margin: 2em; }
       h1 { text-align: center; }
       table { border-collapse: collapse; width: 100%; max-width: 1400px; margin: 1em auto; }
       th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
       th { background-color: #f2f2f2; }
       tr:nth-child(even) { background-color: #f9f9f9; }
       .points { text-align: right; }"]]
    [:body]))

(defn name-and-category
  "Returns 'Race Name' or 'Race Name - Category' if category exists."
  [header]
  (let [category (:category header)
        name (:race-name header)]
    (if category (str name " - <br>" category)
                 name)))

(defn write-races-considered
  "Write a file that shows all the races considered in this last year of scoring. Returns the same races
  so it can be used in a chain"
  [races]
  (let [title "Races Considered"
        date-formatter (f/formatter "yyyy-MM-dd")
        results (keep :header (keep first races))
        sorted-results (sort-by :date t/after? results)]
    (with-open [w (io/writer "content/races-considered.html")]
      (let [html-content
            (hp/html5
              (write-header title)
              [:h1 title]
              [:p "Generated: " (java.time.LocalDateTime/now)
               " — " (count sorted-results) " races"]
              [:table
               [:thead
                [:tr
                 [:th "Race Name"]
                 [:th "Date"]
                 [:th "Points"]]]
               [:tbody
                (for [race sorted-results]
                  (let [name (name-and-category race)
                        date (:date race)
                        date-str (if date
                                   (f/unparse date-formatter date)
                                   "N/A")
                        points (:race-points race)
                        url (:url race)]
                    [:tr
                     [:td [:a {:href url} name]]
                     [:td date-str]
                     [:td.points points]]))]])]
        (.write w html-content)))
    (println "Wrote races-considered.html with" (count sorted-results) "races"))
  races)


(def transformers
  {:date   c/from-string
   :name   trim-and-upper
   :sex    get-sex-from-string
   :gender get-sex-from-string})

(defn value-fn [key value]
  (cond
    ;; Parse date
    (and (= key :date) )
    (c/from-string value)

    (and (= key :name) )
    (trim-and-upper value)

    ;; Convert sex/gender string to keyword
    (and (= key :sex) )
    (get-sex-from-string value)

    ;; Add this if your JSON still has :gender in some files
    (and (= key :gender) )
    (get-sex-from-string value)

    ;; Default: leave everything else unchanged
    :else value))

(defn read-json-race [filename filter-date]
  (with-open [rdr (io/reader filename)]
    (let [header-line (.readLine rdr)
          header (json/read-str header-line :key-fn keyword :value-fn value-fn)]
      (if (filter-date (:date header))
        (->>
          (json/read rdr :key-fn keyword :value-fn value-fn)
          (dedupe-separate-and-score header)
          )
        []))))

(defn read-race [filename fn]
  (cond
    (.endsWith filename ".csv") (read-csv-race filename fn)
    (.endsWith filename ".json") (read-json-race filename fn)
    :else (throw (ex-info "Unsupported file type. Only .csv and .json are supported."
                          {:filename filename}))))

(defn compute-overall-result-sheet []
  (->>
    (scan-directories)
    (map #(read-race % recent-enough?))
    (write-races-considered)
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

(defn add-row-ranks
  "Take the sequence of sorted athletes, break it up into 'tie groups' and alternate
  colors for the groups to make them more clearly visible."
  [sorted-athletes]
  (let [c1 "#d8dbff"
        c2 "#fdf4f8"
        next-color {c1 c2 c2 c1 nil c1}]
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
                (assoc ath :color (next-color (:color prev)))) acc))) nil)
      (reverse))))

(defn make-athlete-table-row
  "Turn an athlete entry to an HTML row using Hiccup.
  this could potentially be done once per athlete, at least for the event cells"
  [athlete]
  (let [athlete-name (:name athlete)
        age (or (:age athlete) "N/A")
        total (format-points (:total athlete))
        events (:events athlete)
        color (name (:color athlete))
        event-cells (map (fn [ev]
                           [:div
                            (name-and-category (:header ev))
                            [:hr]
                            "Points: " (format-points (:points-scored ev))
                            [:br] "rank " (:overall-rank ev)])
                         events)]
    [:tr {:style (str "background-color:" color ";")}
     [:td (str/escape (str (:index athlete) ". " athlete-name)
                      {\& "&amp;" \< "&lt;" \> "&gt;"})]
     [:td age]
     [:td.points total]
     ;; Pad with empty cells up to 10 events
     (for [cell (concat event-cells (repeat 10 ""))]
       [:td (or cell "")])]))

(defn print-to-file
  "Writes athletes to an HTML file as a simple table using Hiccup."
  [athletes sex age-range foreign?]
  (let [sex-str (case sex :male "male" :female "female" (name sex))
        age-str (if (vector? age-range)
                  (str (first age-range) "-" (second age-range))
                  "all")
        foreign-str (if foreign? "all" "us-only")

        base-name (str/join "-" [sex-str age-str foreign-str])
        filename (str "content/" base-name ".html")

        title (str (str/capitalize sex-str) " — "
                   (if age-range
                     (str "Ages " (first age-range) "–" (second age-range))
                     "All Ages")
                   " — " (if foreign? "All Countries" "US Only"))]

    (with-open [w (io/writer filename)]
      (let [html-content
            (h/html
              (write-header title)
              [:h1 title]
              [:p "Generated: " (java.time.LocalDateTime/now)
               " — " (count athletes) " athletes"]
              [:table
               [:thead
                [:tr
                 [:th "Name"]
                 [:th "Age"]
                 [:th "Total Points"]
                 (for [i (range 1 11)]
                   [:th (str "Event " i)])]]
               [:tbody
                (for [athlete (add-row-ranks athletes)]
                  (make-athlete-table-row athlete))]])]
        (.write w html-content)))
    (println "Wrote HTML table to" filename "—" (count athletes) "rows")))

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
        (dorun
          (map (fn [range]
                 (print-both-to-file (filter-ages sorted range) sex range)) age-ranges))))))






