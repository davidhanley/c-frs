(ns c-frs.core-test
  (:require [clojure.test :refer :all]
            [c-frs.core :refer :all]
            [clj-time.coerce :as c]))

(deftest test-fractions
  (testing "fractional seq"
    (is (= (take 4 fractions) [1 5/6 5/7 5/8]))))

(deftest test-score-list
  (testing "score list"
    (is (= (take 3 (get-scores-list 100)) [100 500/6 500/7]))))

(deftest test-dir-scanner
  (testing "dir reader"
    (is (> (count (scan-directories)) 4))))

(deftest test-sex
  (testing "see if gender converter works"
    (is (= (get-sex-from-string "M") :male))
    (is (= (get-sex-from-string "F") :female))
    (is (= (get-sex-from-string "m") :male))
    (is (= (get-sex-from-string "f") :female))
    (is (= (get-sex-from-string "MALE") :male))
    (is (= (get-sex-from-string "FEMALE") :female))
    (is (= (get-sex-from-string "") nil))
    (is (= (get-sex-from-string "ZZ") nil))
    (is (= (get-sex-from-string nil) nil))))

(deftest test-athlete-from-row
  (testing "test athlete from row"
    (let [ath (athlete-from-row ["1", "David Hanley", "50", "male"])]
      (is (= (:name ath) "David Hanley"))
      (is (= (:age ath) 50))
      (is (= (:sex ath) :male))
      )))

(deftest test-normalize-athlete
  (with-redefs [translate-name (fn [name]
                                 ({"DAVE" "DAVID"
                                   "ALICE" "ALICIA"}
                                  name name))
                foreign-name? (fn [name] (= name "ALICIA"))]
    (testing "unchanged name and non-foreign"
      (is (= {:name "BOB" :sex :male :age 42}
             (normalize-athlete {:name "BOB" :sex :male :age 42}))))

    (testing "renamed but non-foreign"
      (is (= {:name "DAVID" :sex :male :age 50}
             (normalize-athlete {:name "DAVE" :sex :male :age 50}))))

    (testing "foreign flag added when translated name matches"
      (is (= {:name "ALICIA" :sex :female :age 31 :foreign true}
             (normalize-athlete {:name "ALICE" :sex :female :age 31}))))))

(deftest test-read-race
  (testing "see if we read a race sanely"
    (let [race (read-csv-race "TowerRunningRaceData/2023-hustle-up-the-hancock.csv" (fn [_] true))
          athlete (first (concat (:male race) (:female race)))
          header (:header race)
          ]
      (is (= (+ (count (:male race)) (count (:female race))) 1244)) ; went down by 1?
      (is (= (:race-name header) "2023 HUSTLE UP THE HANCOCK"))
      (is (= (:race-points header) 150))
      )))


(def hanleys
  [{:name "DAVID HANLEY" :age 30}
   {:name "DAVID HANLEY" :age 50}
   {:name "DAVID HANLEY" :age 51}
   {:name "DAVID HANLEY" :age nil}])


(deftest test-partition
  (testing "see if same-named athletes get separated"
    (let [partitioned-hanleys (partition-athlete hanleys)]
      (is (= (count partitioned-hanleys) 2))
      (is (= partitioned-hanleys [[{:age  nil
                                    :name "DAVID HANLEY"}
                                   {:age  30
                                    :name "DAVID HANLEY"}]
                                  [{:age  50
                                    :name "DAVID HANLEY"}
                                   {:age  51
                                    :name "DAVID HANLEY"}]])))))


(deftest test-parse-name-and-category
  (testing "Basic cases"
    (is (= (parse-name-and-category "sprint")
           {:race-name "sprint"}))
    (is (= (parse-name-and-category "race - sprint")
           {:race-name "race" :category "sprint"}))
    (is (= (parse-name-and-category "100m hurdles - men")
           {:race-name "100m hurdles" :category "men"})))

  (testing "Extra spaces around dash"
    (is (= (parse-name-and-category "  triathlon  -  olympic  ")
           {:race-name "triathlon" :category "olympic"}))
    (is (= (parse-name-and-category "swim - 50m butterfly ")
           {:race-name "swim" :category "50m butterfly"}))))



;; Helper to make tests cleaner
(defn race [name points date & {:keys [age overall-rank]}]
  {:header        {
                   :race-name name
                   :date      date
                   }
   :points-scored points
   :sex           :female
   :name          "JILL PAHA"
   :age           age
   :overall-rank  overall-rank})

; deduplicate an athlete's races, sorting them by highest to lowest points.

(deftest test-deduplicate-by-race-max-points
  (testing "Basic deduplication - keeps highest points per race"
    (let [input [(race "2022 T2T TAMPA" 150 1665273600000 :age 41)
                 (race "2022 T2T TAMPA" 120 1665273700000 :age 41)
                 (race "SCALE THE STRAT" 250 1646611200000 :age 41)
                 (race "SCALE THE STRAT" 200 1646611300000 :age 41)]
          expected [{:header {:race-name "SCALE THE STRAT" :date 1646611200000} :points-scored 250 :age 41 :sex :female :name "JILL PAHA" :overall-rank nil}
                    {:header {:race-name "2022 T2T TAMPA" :date 1665273600000} :points-scored 150 :age 41 :sex :female :name "JILL PAHA" :overall-rank nil}
                    ]]
      (is (= expected (deduplicate-by-race-max-points input)))))

  (testing "Preserves other fields from the winning entry"
    (let [input [(race "WILLIS" 200 1635292800000 :age 40 :overall-rank 1)
                 (race "WILLIS" 180 1635292900000 :age 41 :overall-rank 3)
                 (race "WILLIS" 500/3 1635293000000 :age nil :overall-rank 2)] ;; 166.666... < 200
          expected [{:header {:race-name "WILLIS" :date 1635292800000} :points-scored 200 :age 40 :sex :female :name "JILL PAHA" :overall-rank 1}]]
      (is (= expected (deduplicate-by-race-max-points input)))))

  (testing "Handles ratios correctly (max-key compares them numerically)"
    (let [input [(race "2021 WILLIS" 500/3 1635292800000)   ;; ≈166.67
                 (race "2021 WILLIS" 150 1635292900000)
                 (race "2021 WILLIS" 200 1635293000000)]
          expected [{:header {:race-name "2021 WILLIS" :date 1635293000000} :points-scored 200 :sex :female :name "JILL PAHA" :age nil :overall-rank nil}]]
      (is (= expected (deduplicate-by-race-max-points input)))))

  (testing "Empty input and single entry"
    (is (= [] (deduplicate-by-race-max-points [])))
    (let [single [(race "ONLY ONE" 250 1665014400000 :overall-rank 1)]]
      (is (= single (deduplicate-by-race-max-points single)))))

  (testing "No duplicates → returns input sorted by points descending"
    (let [input [(race "RACE B" 200 1)
                 (race "RACE C" 150 2)
                 (race "RACE A" 100 3)]
          result (deduplicate-by-race-max-points input)]
      (is (= input result))))

  (testing "Realistic multi-duplicate case from your style"
    (let [input [(race "2022 T2T TAMPA" 150 1665273600000)
                 (race "2022 T2T TAMPA" 100 1665273700000)
                 (race "SCALE THE STRAT" 250 1646611200000)
                 (race "2022 SOUTHFIELD SINGLE CLIMB" 50 1668124800000)
                 (race "2022 SOUTHFIELD SINGLE CLIMB" 40 1668124900000)]
          expected [{:header {:race-name "SCALE THE STRAT" :date 1646611200000} :points-scored 250 :sex :female :name "JILL PAHA" :overall-rank nil :age nil}
                    {:header {:race-name "2022 T2T TAMPA" :date 1665273600000} :points-scored 150 :sex :female :name "JILL PAHA" :overall-rank nil :age nil}
                    {:header {:race-name "2022 SOUTHFIELD SINGLE CLIMB" :date 1668124800000} :points-scored 50 :sex :female :name "JILL PAHA" :overall-rank nil :age nil}]]
      (is (= expected (deduplicate-by-race-max-points input))))))


;; Helper to create sample race maps
(defn race2 [points age]
  {:points-scored points
   :age           age
   :name          (str "Race-" (rand-int 1000))})           ; just for uniqueness

(deftest compute-points-to-use-test

  (testing "real entry that fails"
    (let [races [{:race-name "US BANK LA 2025", :date 1758585600000, :race-points 150, :name "DAVID HANLEY", :sex :male, :age 52, :points-scored 375/46, :overall-rank 88}
                 {:race-name "2026 OMAHA VERTICAL MILE", :date 1769904000000, :race-points 150, :name "DAVID HANLEY", :sex :male, :age 53, :points-scored 750/19, :overall-rank 15}
                 {:race-name "2024 BASE 2 SPACE", :date 1727568000000, :race-points 150, :name "DAVID HANLEY", :sex :male, :age nil, :points-scored 375/16, :overall-rank 28}
                 {:category "- US CHAMPIONSHIP", :date 1740268800000, :points-scored 625/38, :age 52, :sex :male, :name "DAVID HANLEY", :overall-rank 72, :race-name "2025 HUSTLE", :race-points 250}
                 {:race-name "2025 WILLIS CLIMB", :date 1762041600000, :race-points 200, :name "DAVID HANLEY", :sex :male, :age 52, :points-scored 10N, :overall-rank 96}
                 {:race-name "2024 US BANK LA", :date 1727395200000, :race-points 150, :name "DAVID HANLEY", :sex :male, :age 51, :points-scored 250/13, :overall-rank 35} {:race-name "2023 US BANK LA", :date 1695945600000, :race-points 200, :name "DAVID HANLEY", :sex :male, :age 50, :points-scored 1000/49, :overall-rank 45}]
          result (create-athlete-row races)]
      (is (= 681795/6992 (:total result)) "Should sum top 5 points: 100+200+150+300+50")
      (is (= 53 (:age result)) "Should find max age = 53")
      (is (= 7 (count (:events result))) "Should keep all events")))

  (testing "Normal case - more than 5 races, mixed ages"
    (let [races [(race2 100 35)
                 (race2 200 nil)
                 (race2 150 36)
                 (race2 300 34)
                 (race2 50 nil)
                 (race2 400 37)
                 (race2 80 35)]
          result (create-athlete-row races)]
      (is (= 800 (:total result)) "Should sum top 5 points: 100+200+150+300+50")
      (is (= 37 (:age result)) "Should find max age = 37")
      (is (= 7 (count (:events result))) "Should keep all events")))

  (testing "Fewer than 5 races"
    (let [races [(race2 120 28)
                 (race2 180 29)
                 (race2 90 nil)]
          result (create-athlete-row races)]
      (is (= 390 (:total result)) "Should sum all 3")
      (is (= 29 (:age result)) "Max age among known ages")
      (is (= 3 (count (:events result))))))

  (testing "All ages are nil → :age should be nil"
    (let [races [(race2 100 nil)
                 (race2 200 nil)
                 (race2 150 nil)]
          result (create-athlete-row races)]
      (is (= 450 (:total result)))
      (is (nil? (:age result)) "No known ages → :age nil")))

  (testing "Empty input"
    (let [result (create-athlete-row [])]
      (is (= 0 (:total result)) "Sum of no points = 0")
      (is (nil? (:age result)) "No ages → nil")
      (is (empty? (:events result)))))

  (testing "Only one race"
    (let [races [(race2 500 33)]
          result (create-athlete-row races)]
      (is (= 500 (:total result)))
      (is (= 33 (:age result)))))
  )

(deftest name-translator-test
  (fn []
    (let [translate (name-translator-factory)]

      (testing "exact matches"
        (is (= "CHERYL LEONARD-SCHNECK"
               (:name (translate {:name "SHERYL LEONARD SCHNECK"}))))
        (is (= "CHERYL LEONARD-SCHNECK"
               (:name (translate {:name "SHERYL LEONARD SCHNEC"}))))
        (is (= "david hanley"
               (:name (translate {:name "dave hanley"}))))
        (is (= "robert klinko"
               (:name (translate {:name "bob klinko"}))))
        (is (= "SOH WAI CHING"
               (:name (translate {:name "WAI CHING SOH"})))))

      (testing "wildcard .* matches"
        (is (= "madeline fontillas-ronk"
               (:name (translate {:name "madeline ronk"}))))
        (is (= "madeline fontillas-ronk"
               (:name (translate {:name "madeline Xronk"}))))
        (is (= "Natalie DOOLITTLE-shadel"
               (:name (translate {:name "Natalie DOOLITTLE anything here"}))))
        (is (= "MARIA ELISA LOPEZ PIMENTEL"
               (:name (translate {:name "MARIA ELISA LOPEZ P. whatever"}))))
        (is (= "ARTURO VELAZQUEZ LEYVA"
               (:name (translate {:name "ARTURO VELAZQUEZ XYZ"})))))

      (testing "prefix/suffix style matches"
        (is (= "SHERYL LEONARD-SCHNECK"
               (:name (translate {:name "SHERYL LEONARD-SC extra stuff"})))))

      (testing "no match → name unchanged"
        (is (= "unknown person"
               (:name (translate {:name "unknown person"}))))
        (is (= "Dave Hanley"                                ; different case → no match
               (:name (translate {:name "Dave Hanley"})))))

      (testing "edge cases"
        (is (= {:name nil} (translate {:name nil})))
        (is (= {} (translate {})))
        (is (= {:other :data} (translate {:other :data})))))))

(def sample-athletes
  [{:name "A" :age 18}
   {:name "B" :age 20}
   {:name "C" :age 29}
   {:name "D" :age 30}
   {:name "E" :age 45}
   {:name "F" :age nil}
   {:name "G" :age 19}
   {:name "H" :age 100}
   {:name "I" :age 200}])

(deftest filter-ages-test
  (testing "standard ranges from age-ranges list"
    (is (= ["A" "G"]
           (map :name (filter-ages sample-athletes [0 19])))
        "0–19 includes 18 and 19")

    (is (= ["B" "C"]
           (map :name (filter-ages sample-athletes [20 29])))
        "20–29 includes 20 and 29")

    (is (= ["D"]
           (map :name (filter-ages sample-athletes [30 39])))
        "30–39 includes 30")

    (is (= ["E"]
           (map :name (filter-ages sample-athletes [40 49])))
        "40–49 includes 45"))

  (testing "boundary behavior (inclusive)"
    (is (= ["A" "G"]
           (map :name (filter-ages sample-athletes [18 19])))
        "exact boundary values are included")

    (is (= ["B"]
           (map :name (filter-ages sample-athletes [20 20])))
        "single value range includes exact match"))

  (testing "nil ages are excluded"
    (is (not-any? #(= (:name %) "F")
                  (filter-ages sample-athletes [0 19]))
        "nil age should never match"))

  (testing "high-end ranges"
    (is (= ["H" "I"]
           (map :name (filter-ages sample-athletes [100 200])))
        "100–200 includes 100")

    (is (empty?
          (filter-ages sample-athletes [101 199]))
        "nothing matches 101–199"))

  (testing "edge cases"
    (is (empty? (filter-ages [] [0 19]))
        "empty collection returns empty")

    (is (empty? (filter-ages [{:name "X" :age nil}] [20 30]))
        "only nil ages → empty result")

    (is (= ["A"]
           (map :name (filter-ages [{:name "A" :age 18}] [0 19])))
        "single matching athlete")

    (is (= ["B"]
           (map :name (filter-ages [{:name "B" :age 20} {:name "Y" :age nil}] [20 29])))
        "filters out nil even when others match")))

(deftest test-add-standard-ranks
  (testing "Standard 1224 ranking cases"

    (testing "Mixed ties – 1, then 3-way tie, then 2-way tie, then singles"
      (let [athletes [{:name "A" :total 100}
                      {:name "B" :total 90}
                      {:name "C" :total 90}
                      {:name "D" :total 90}
                      {:name "E" :total 80}
                      {:name "F" :total 80}
                      {:name "G" :total 70}
                      {:name "H" :total 50}]
            sorted (sort-by :total > athletes)
            ranked (add-row-ranks sorted)
            ranks (map :index ranked)]
        (is (= [1 2 2 2 5 5 7 8] ranks))))

    (testing "All unique → straight 1,2,3,4…"
      (let [athletes (map #(hash-map :name (str "P" %) :total (- 100 %)) (range 1 6))
            ranks (->> athletes (sort-by :total >) add-row-ranks (map :index))]
        (is (= [1 2 3 4 5] ranks))))

    (testing "All athletes tied → everyone gets rank 1"
      (let [athletes (repeat 5 {:name "Tied" :total 88})
            ranks (->> athletes (sort-by :total >) add-row-ranks (map :index))]
        (is (every? #{1} ranks))))

    (testing "Three-way tie at the top → next is 4th"
      (let [athletes [{:name "Gold1" :total 95}
                      {:name "Gold2" :total 95}
                      {:name "Gold3" :total 95}
                      {:name "4th" :total 90}
                      {:name "5th" :total 85}]
            ranks (->> athletes (sort-by :total >) add-row-ranks (map :index))]
        (is (= [1 1 1 4 5] ranks))))

    (testing "Single athlete"
      (is (= [1] (->> [{:name "Solo" :total 42}]
                      (sort-by :total >)
                      add-row-ranks
                      (map :index)))))

    (testing "Empty list → empty result"
      (is (= [] (add-row-ranks []))))))

(deftest test-dedupe-athletes
  (testing "keeps first occurrence of each name and preserves order"
    (let [athletes [{:name "Alice" :sex :female :age 25 :points-scored 1200}
                    {:name "Bob"   :sex :male   :age 30 :points-scored 1100}
                    {:name "Alice" :sex :female :age 25 :points-scored 1350}  ; duplicate, should be dropped
                    {:name "Charlie" :sex :male :age 28 :points-scored 900}
                    {:name "Bob"   :sex :male   :age 30 :points-scored 950}   ; duplicate
                    {:name "Diana" :sex :female :age 22 :points-scored 1400}]]

      (is (= [{:name "Alice" :sex :female :age 25 :points-scored 1200}
              {:name "Bob"   :sex :male   :age 30 :points-scored 1100}
              {:name "Charlie" :sex :male :age 28 :points-scored 900}
              {:name "Diana" :sex :female :age 22 :points-scored 1400}]
             (dedupe-athletes athletes)))))

  (testing "handles empty list"
    (is (= [] (dedupe-athletes []))))

  (testing "handles no duplicates"
    (let [athletes [{:name "Alice" :sex :female}
                    {:name "Bob"   :sex :male}]]
      (is (= athletes (dedupe-athletes athletes))))))

(deftest test-json-read
  (testing "see if reading a json race works"
    (let [athletes (read-json-race "TowerRunningRaceData/2026-oakbrook-single.json" (fn [x]true))
          first-ath (first (concat (:male athletes) (:female athletes)))
          header (:header athletes)]
         (is (= (:race-name header) "Oakbrook 2026"))
         (is (= (:race-points header) 50))
         (is (= (:date header) (c/from-string "2026-3-8")))
         (is (= (+ (count (:male athletes)) (count (:female athletes))) 338))
         (is (= (:name first-ath) "THOMAS BAKER"))
         (is (= (:url header) "https://github.com/davidhanley/TowerRunningRaceData/blob/main/2026-oakbrook-single.json"))
         ))

  (testing "see if a date out or range results in empty results"
    (let [athletes (read-json-race "TowerRunningRaceData/2026-ffa-orlando.json" (fn [x]false))]
      (is (nil? athletes))
      ))
  )

(deftest us-only-scoring-reranks-after-foreign-removal
  (testing "US-only scoring recomputes rank/points within each race"
    (let [race-data [{:header {:race-name "TEST RACE" :race-points 150 :date (c/from-string "2026-01-01")}
                      :male   [{:name "FOREIGN WINNER" :sex :male :foreign true}
                               {:name "TOP AMERICAN" :sex :male}
                               {:name "SECOND AMERICAN" :sex :male}]
                      :female []}]]
      (let [all-results (:male (#'c-frs.core/compute-overall-result-sheet-from-races race-data true))
            us-results  (:male (#'c-frs.core/compute-overall-result-sheet-from-races race-data false))
            all-top-us  (first (filter #(= "TOP AMERICAN" (:name %)) all-results))
            us-top-us   (first (filter #(= "TOP AMERICAN" (:name %)) us-results))
            all-event   (first (:events all-top-us))
            us-event    (first (:events us-top-us))]
        (is (= 125 (:points-scored all-event)) "Second overall in all-country scoring")
        (is (= 2 (:overall-rank all-event)))
        (is (= 150 (:points-scored us-event)) "Promoted to first in US-only scoring")
        (is (= 1 (:overall-rank us-event)))))))
