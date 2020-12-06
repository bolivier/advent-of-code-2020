(ns aoc.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn input []
  (slurp (io/resource "day-6.input")))

(defn split-input [input]
  (map
   #(str/split % #"\n")
   (str/split input #"\n\n")))

(defn solve []
  (->> (input)
       split-input
       (map str/join)
       (map set)
       (map count)
       (apply +)))

(defn solve-2 []
  (->> (input)
       split-input
       (map #(map set %))
       (map #(apply set/intersection %))
       (map count)
       (apply +)))

(comment
  (def input "abc

a
b
c

ab
ac

a
a
a
a

b")
  nil)
