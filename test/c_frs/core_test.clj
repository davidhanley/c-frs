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
    (let [race (read-race "TowerRunningRaceData/2021-esbru.csv" #(if % true true))
          first-race (first race)
          ]
      (doseq [ath race] (prn ath))

      )))

(def hanleys
  [{:name "DAVID HANLEY" :age 30 }
   {:name "DAVID HANLEY" :age 50 }
   {:name "DAVID HANLEY" :age 52 }
   {:name "DAVID HANLEY" :age nil}])

(deftest test-partition
  (testing "see if same-named athletes get separated"
    (let [partitioned-hanleys (partition-athlete hanleys)]
      (is (= (count partitioned-hanleys) 3)

          ))))


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