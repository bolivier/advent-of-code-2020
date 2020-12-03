(ns aoc.day-1-test
  (:require [aoc.day-1 :as sut]
            [clojure.test :refer [deftest is]]))

(deftest day-1-sample
  (is (= 514579
         (sut/multiply-expenses
          [1721
           979
           366
           299
           675
           1456]))))
