(ns c-frs.core-test
  (:require [clojure.test :refer :all]
            [c-frs.core :refer :all]))

(deftest test-fractions
  (testing "fractional seq"
    (is (= (take 4 fractions) [1 5/6 5/7 5/8]))))

(deftest test-score-list
  (testing "score list"
    (is (= (take 3 (get-scores-list 100)) [100 500/6 500/7]))))

(deftest read-file-into-test
  (testing "file reader"
    (let [pf (fn [x] (doall (map println x)))]
           (read-file-into "README.md" pf))))
