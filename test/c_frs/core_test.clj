(ns c-frs.core-test
  (:require [clojure.test :refer :all]
            [c-frs.core :refer :all]))

(deftest test-fractions
  (testing "fractional seq"
    (is (= (take 4 fractions) [1 5/6 5/7 5/8]))))

(deftest test-score-list
  (testing "score list"
    (is (= (take 3 (get-scores-list 100)) [100 500/6 500/7]))))

(deftest test-dir-scanner
  (testing "dir reader"
    (is (> (count (scan-directories)) 4))))

(deftest test-gender
  (testing "see if gender converter works"
    (is (= (get-sex-from-string "M") :male))
    (is (= (get-sex-from-string "F") :female))
    (is (= (get-sex-from-string "m") :male))
    (is (= (get-sex-from-string "f") :female))
    (is (= (get-sex-from-string "MALE") :male))
    (is (= (get-sex-from-string "FEMALE") :female))
    (is (= (get-sex-from-string "") nil))
    (is (= (get-sex-from-string "ZZ") nil))
    ))

(deftest test-athlete-from-row
  (testing "test athlete from row"
    (let [ath (athlete-from-row ["1", "David Hanley", "50", "male"])]
      (is (= (:name ath) "David Hanley"))
      (is (= (:age ath) 50))
      (is (= (:sex ath) :male))
      )))


(def aths
  [{:race-name "2021 ESBRU", :date 1635292800000, :race-points 200, :athlete-name "LYNDA LIMERICK", :gender :female, :age 50}
   {:race-name "2021 ESBRU", :date 1635292800000, :race-points 200, :athlete-name "NICHOLAS WILSON", :gender :male, :age 29}
   {:race-name "2021 ESBRU", :date 1635292800000, :race-points 200, :athlete-name "JEFF HANGER", :gender :male, :age 56}
   {:race-name "2021 ESBRU", :date 1635292800000, :race-points 200, :athlete-name "MYKOLA ZAVADA", :gender :male, :age 40}])

(deftest test-read-race
  (testing "see if we read a race sanely"
    (let [race (read-race "TowerRunningRaceData/2023-hustle-up-the-hancock.csv" #(if % true true))
          first-race (first race)
          ]
      (doseq [ath race] (prn ath))

      )))

(def hanleys
  [{:name "DAVID HANLEY" :age 30}
   {:name "DAVID HANLEY" :age 50}
   {:name "DAVID HANLEY" :age 51}
   {:name "DAVID HANLEY" :age nil}])


(deftest test-partition
  (testing "see if same-named athletes get separated"
    (let [partitioned-hanleys (partition-athlete hanleys)]
      (println partitioned-hanleys)
      (is (= (count partitioned-hanleys) 2))
      (is (= partitioned-hanleys [[{:age  30                ; grouped athlete 1
                                    :name "DAVID HANLEY"}]
                                  [{:age  50                ; grouped athlete 2
                                    :name "DAVID HANLEY"}
                                   {:age  51
                                    :name "DAVID HANLEY"}
                                   {:age  nil
                                    :name "DAVID HANLEY"}]]))
      )))


(deftest test-parse-name-and-category
  (testing "Basic cases"
    (is (= (parse-name-and-category "sprint")
           {:name "sprint"}))
    (is (= (parse-name-and-category "race - sprint")
           {:name "race" :category "sprint"}))
    (is (= (parse-name-and-category "100m hurdles - men")
           {:name "100m hurdles" :category "men"})))

  (testing "Extra spaces around dash"
    (is (= (parse-name-and-category "  triathlon  -  olympic  ")
           {:name "triathlon" :category "olympic"}))
    (is (= (parse-name-and-category "swim - 50m butterfly ")
           {:name "swim" :category "50m butterfly"}))))



;; Helper to make tests cleaner
(defn race [name points date & {:keys [age overall-rank]}]
  {:race-name    name
   :points       points
   :date         date
   :sex          :female
   :name         "JILL PAHA"
   :age          age
   :overall-rank overall-rank})

; deduplicate an athlete's races, sorting them by highest to lowest points.

(deftest test-deduplicate-by-race-max-points
  (testing "Basic deduplication - keeps highest points per race"
    (let [input [(race "2022 T2T TAMPA" 150 1665273600000 :age 41)
                 (race "2022 T2T TAMPA" 120 1665273700000 :age 41)
                 (race "SCALE THE STRAT" 250 1646611200000 :age 41)
                 (race "SCALE THE STRAT" 200 1646611300000 :age 41)]
          expected [{:race-name "SCALE THE STRAT" :points 250 :date 1646611200000 :age 41 :sex :female :name "JILL PAHA" :overall-rank nil}
                    {:race-name "2022 T2T TAMPA" :points 150 :date 1665273600000 :age 41 :sex :female :name "JILL PAHA" :overall-rank nil}
                    ]]
      (is (= expected (deduplicate-by-race-max-points input)))))

  (testing "Preserves other fields from the winning entry"
    (let [input [(race "WILLIS" 200 1635292800000 :age 40 :overall-rank 1)
                 (race "WILLIS" 180 1635292900000 :age 41 :overall-rank 3)
                 (race "WILLIS" 500/3 1635293000000 :age nil :overall-rank 2)] ;; 166.666... < 200
          expected [{:race-name "WILLIS" :points 200 :date 1635292800000 :age 40 :sex :female :name "JILL PAHA" :overall-rank 1}]]
      (is (= expected (deduplicate-by-race-max-points input)))))

  (testing "Handles ratios correctly (max-key compares them numerically)"
    (let [input [(race "2021 WILLIS" 500/3 1635292800000)   ;; ≈166.67
                 (race "2021 WILLIS" 150 1635292900000)
                 (race "2021 WILLIS" 200 1635293000000)]
          expected [{:race-name "2021 WILLIS" :points 200 :date 1635293000000 :sex :female :name "JILL PAHA" :age nil :overall-rank nil}]]
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
          expected [{:race-name "SCALE THE STRAT" :points 250 :date 1646611200000 :sex :female :name "JILL PAHA" :overall-rank nil :age nil}
                    {:race-name "2022 T2T TAMPA" :points 150 :date 1665273600000 :sex :female :name "JILL PAHA" :overall-rank nil :age nil}
                    {:race-name "2022 SOUTHFIELD SINGLE CLIMB" :points 50 :date 1668124800000 :sex :female :name "JILL PAHA" :overall-rank nil :age nil}]]
      (is (= expected (deduplicate-by-race-max-points input))))))