(ns aoc-2020.core-test
  (:require [clojure.test :refer :all]
            [aoc-2020.core :refer :all]))

(deftest day1-test-part1
  (testing "That we can find two numbers in a set of numbers that sum to 2020")
    (is (= #{1721 299} (set (as-sum-of-n 2 2020 '(1721 979 366 299 675 1456))))))

(deftest day1-test-part2
  (testing "That we can find three numbers in a set of numbers that sum to 2020")
    (is (= #{979 366 675} (set (as-sum-of-n 3 2020 '(1721 979 366 299 675 1456))))))


