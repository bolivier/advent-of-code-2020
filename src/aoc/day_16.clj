(ns aoc.day-16
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def filename "day-16.input")

(defn ->range-pred [s]
  (let [[lower higher] (map utils/parse-int (str/split s #"-"))]
    #(<= lower % higher)))

(defn ->pred [field-def]
  (apply some-fn
         (map
          ->range-pred
          (str/split
           (str/trim (second
                      (str/split field-def #":")))
           #" or "))))

(defn field-validator [field-defs]
  (apply some-fn (map ->pred field-defs)))

(defn parse-ticket [raw-ticket]
  (map utils/parse-int (str/split raw-ticket #",")))

(defn solve []
  (with-open [rdr (utils/input-reader filename)]
    (let [input (line-seq rdr)
          [field-defs ticket nearby-tickets] (utils/partition-by-blank-lines input)
          validator (field-validator field-defs)
          tickets (mapcat parse-ticket (drop 1 nearby-tickets))]
      (apply + (remove
                validator
                tickets)))))

[["class: 1-3 or 5-7" "row: 6-11 or 33-44" "seat: 13-40 or 45-50"]
 ["your ticket:" "7,1,14"]
 ["nearby tickets:" "7,3,47" "40,4,50" "55,2,20" "38,6,12"]]


(comment
  (def s "1-3")
  (def field-def "class: 1-3 or 5-7")
  (def filename "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")
  nil)
