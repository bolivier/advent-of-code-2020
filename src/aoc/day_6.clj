(ns aoc.day-6
  (:require [aoc.utils :as utils]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def filename "day-6.input")

(defn solve []
  (with-open [lines (utils/input-reader filename)]
    (transduce
     (comp utils/blank-line-splitting
           (map str/join)
           (map set)
           (map count))
     +
     (line-seq lines))))

(defn solve-2 []
  (with-open [lines (utils/input-reader filename)]
    (transduce
     (comp (map set)
           utils/blank-line-splitting
           (map #(apply set/intersection %))
           (map count))
     +
     (line-seq lines))))

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
