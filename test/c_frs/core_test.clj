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
    (is (= (get-gender-from-string "M") :male))
    (is (= (get-gender-from-string "F") :female))
    (is (= (get-gender-from-string "MALE") :male))
    (is (= (get-gender-from-string "FEMALE") :female))
    (is (= (get-gender-from-string "") nil))
    (is (= (get-gender-from-string "ZZ") nil))
    ))

(deftest test-athlete-from-row
  (testing "test athlete from row"
    (let [ath (athlete-from-row ["1","David Hanley","50","Male"])])))


(deftest test-read-race
  (testing "see if we read a race sanely"
    (let [race (read-race "TowerRunningRaceData/2021-esbru.csv" #(if % true true))
          first-race (first race)
          ]
      (doseq [ath race]
        (println ath))

      )))