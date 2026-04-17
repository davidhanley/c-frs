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

(defn get-sex-from-string
  "Given a string describing the athlete's sex, reutrn :male or :female"
  [sex-string]
  (some-> sex-string
          (str/replace "*" "")
          first
          Character/toUpperCase
          {\M :male \F :female}))

(defn safe-parse-int [s]
  (some-> s clojure.string/trim parse-long))

(defn process-lines
  "Applies f to each non-blank, non-comment line.
   f receives the trimmed line as string."
  [filepath f]
   (with-open [rdr (io/reader filepath)]
     (->> (line-seq rdr)
          (map str/trim)
          (remove #(or (str/blank? %) (str/starts-with? % "#")))
          (map str/upper-case)
          (map f)
          doall)))                                         ; force realization before closing file

(defn name-translator-factory []
  (let [rules (process-lines "TowerRunningRaceData/translate.dat"
                             (fn [line]
                               (let [[pattern name] (str/split line #"\s*,\s*" 2)]
                                 [(re-pattern pattern) name])))]
    (fn [name]
      (or (some (fn [[re repl]]
                  (when (re-matches re name)
                    repl))
                rules)
          name))))

(defn foreign-marker-factory
  "Reads TowerRunningRaceData/foreign.dat and returns a function that
   returns true when an athlete name exactly matches a line in the file."
  []
  (let [foreign-names (set (process-lines "TowerRunningRaceData/foreign.dat" identity))]
    (fn [name]
      (contains? foreign-names name))))

(def translate-name (name-translator-factory))
(def foreign-name? (foreign-marker-factory))

(defn normalize-athlete [athlete]
  (let [old-name (:name athlete)
        new-name (translate-name old-name)
        foreign? (foreign-name? new-name)]
    (cond-> athlete
            (not= old-name new-name) (assoc :name new-name)
            foreign? (assoc :foreign true))))

(defn athlete-from-row
  "given an array of strings from the CSV row, make an athlete struct"
  [row]
  (let [[_ raw-name age-str sex-str] row]
    (normalize-athlete {:name raw-name
                        :sex  (get-sex-from-string sex-str)
                        :age  (safe-parse-int age-str)})))

(def parse-date c/from-string)

(defn parse-name-and-category
  "get the race name into a struct, with an option category if there is a dash"
  [race-description]
  (zipmap [:race-name :category]
          (map str/trim (str/split race-description #"\s*-\s*" 2))))

(defn to-url
  "Create a URL string from the filename so users can see the data in github"
  [filename]
  (str "https://github.com/davidhanley/TowerRunningRaceData/blob/main/"
       (last (str/split filename #"/"))))

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

(defn add-scores-and-rank
  "Given a sequence of athletes and the race header, add the header, and the score and rank for each athlete"
  [athletes header]
  (map #(assoc %1 :header header :points-scored %2 :overall-rank (inc %3)) athletes (get-scores-list (:race-points header)) (range)))

(defn dedupe-separate
  "Dedupe athletes, separate by gender, and keep race results unscored."
  [header athletes]
  (let [{:keys [male female]} (group-by :sex (dedupe-athletes athletes))]
    {:header header
     :male   (vec male)
     :female (vec female)}))

(defn get-race-from-csv-strings
  "Given a sequence of string, parse a race from that. The structure of a race is all the athletes who competed.
  As we operate on athletes, each athlete has a copy of the race info, points, date, name, etc."
  [sheet-strings date-filter filename]
  (let [[namestr datestr _ pointsstr & rest] sheet-strings
        date (parse-date (first datestr))
        points (safe-parse-int (first pointsstr))
        header (conj (parse-name-and-category (first namestr)) {:date date :race-points points :url (to-url filename)})
        ]
    (when (and points date (date-filter date))
      (dedupe-separate header (map athlete-from-row rest)))))

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

(defn read-csv-race
  "Map the race-to-strings function over the contents of the race file"
  [filename keep-race?]
  (read-csv-file-into filename #(get-race-from-csv-strings % keep-race? filename)))

(defn partition-when
  "Like partition-by but decides split based on consecutive pairs"
  [should-continue? coll]
  (if (seq coll)
    (reduce
      (fn [acc x]
        (if (or (empty? acc)
                (should-continue? (last (last acc)) x))
          (update acc (dec (count acc)) conj x)
          (conj acc [x])))
      [[(first coll)]]
      (next coll))
    []))

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

(defn create-athlete-row
  "create the result row struct, age, total points, name, event, etc"
  ; todo: render the HTML row here?
  [races]
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

(def one-year-ago
  (t/minus (t/now) (t/years 1)))

(defn recent-enough? [race-date]
  (t/after? race-date one-year-ago))


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
        results (keep :header races)
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
  (let [transform (get transformers key identity)]
    (transform value)))

(defn read-json-race
  "Given a JSON file, read it into a race result.  Much simpler than the CSV reads"
  [filename filter-date]
      (println "reading " filename)
  (with-open [rdr (io/reader filename)]
    (let [header-line (.readLine rdr)
          header (conj  (json/read-str header-line :key-fn keyword :value-fn value-fn) {:url (to-url filename)})]
      (when (filter-date (:date header))
        (->>
          (json/read rdr :key-fn keyword :value-fn value-fn)
          (map normalize-athlete)
          (dedupe-separate header))))))

(defn read-race
  [filename keep-race?]
  (cond
    (.endsWith filename ".csv") (read-csv-race filename keep-race?)
    (.endsWith filename ".json") (read-json-race filename keep-race?)
    :else (throw (ex-info "Unsupported file type. Only .csv and .json are supported."
                          {:filename filename}))))

(defn- compute-sex-results
  "Build the athlete result sheet for one sex from race maps.
   If include-foreign? is false, race scoring is recomputed after foreign athletes are removed."
  [races sex include-foreign?]
  (->> races
       (mapcat (fn [race]
                 (let [header (:header race)
                       race-athletes (->> (sex race)
                                          (filter #(or include-foreign? (not (:foreign %)))))]
                   (add-scores-and-rank race-athletes header))))
       (group-by :name)
       (mapcat rest)
       (mapcat partition-athlete)
       (map deduplicate-by-race-max-points)
       (map create-athlete-row)
       (vec)))

(defn- load-recent-races []
  (->>
    (scan-directories)
    (keep #(read-race % recent-enough?))
    (write-races-considered)))

(defn- compute-overall-result-sheet-from-races [races include-foreign?]
  {:male   (compute-sex-results races :male include-foreign?)
   :female (compute-sex-results races :female include-foreign?)})

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
  "Writes pre-ranked athletes to an HTML file as a simple table using Hiccup."
  [athletes sex age-range foreign?]
  (let [sex-str (name sex)
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
                (for [athlete athletes]
                  (make-athlete-table-row athlete))]])]
        (.write w html-content)))
    (println "Wrote HTML table to" filename "—" (count athletes) "rows")))

(def age-ranges [[0 19] [20 29] [30 39] [40 49] [50 59] [60 69] [70 79] [80 89] [90 99] [100 200]])

(defn filter-ages [athletes [min-age max-age]]
  (filter (fn [athlete]
            (let [age (:age athlete)]
              (and age (<= age max-age) (>= age min-age))))
          athletes))

(defn make-sheets
  "Generate all output sheets; intended for REPL use."
  []
  (let [races (load-recent-races)
        results-by-mode
        {true  (compute-overall-result-sheet-from-races races true)
         false (compute-overall-result-sheet-from-races races false)}]
    (doseq [sex [:male :female]
            foreign? [true false]]
      (let [athletes (get-in results-by-mode [foreign? sex])
            sorted (sort-by :total > athletes)]
        (print-to-file (add-row-ranks sorted) sex nil foreign?)
        (dorun
          (pmap (fn [range]
                 (print-to-file (add-row-ranks (filter-ages sorted range)) sex range foreign?))
               age-ranges))))))

(defn -main
  [& _args]
  (make-sheets)
  (shutdown-agents))

