(ns aoc.day-16-test
  (:require [aoc.day-16 :as sut]
            [clojure.test :refer [deftest is testing]]))

(deftest validator
  (testing "predicate builder"
    (let [pred (sut/->pred "class: 1-3 or 8-9")]
      (is (true?
           (pred 3)))))
  (testing "validator"
    (let [field-defs ["class: 1-3 or 5-7" "row: 6-11 or 33-44" "seat: 13-40 or 45-50"]
          validator (sut/field-validator field-defs)]
      validator)))
