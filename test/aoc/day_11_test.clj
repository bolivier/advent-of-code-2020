(ns aoc.day-11-test
  (:require [aoc.day-11 :as sut]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is testing]]
            [aoc.utils :as utils]))

(def empty-iteration
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(def first-iteration
  "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##")

(def second-iteration
  "#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##")

(deftest get-new-adjacent-seats-test
  (testing "Simple case"
    (is
     (= 3
        (count (filter sut/seat?
                       (sut/get-adjacent (sut/parse (utils/input-reader second-iteration))
                                         0
                                         0)))))))
