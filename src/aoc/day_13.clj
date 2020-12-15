(ns aoc.day-13
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn min-with [f coll]
  (reduce
   (fn [min-element curr-element]
     (if (< (f curr-element) (f min-element))
       curr-element
       min-element))
   coll))

(defn max-with [f coll]
  (reduce
   (fn [min-element curr-element]
     (if (< (f min-element) (f curr-element))
       curr-element
       min-element))
   coll))

(def filename "day-13.input")

(defn solve []
  (with-open [rdr (utils/input-reader filename)]
    (let [input (line-seq rdr)
          earliest-departure-time (utils/parse-int (first input))
          bus-defs (second input)
          buses (map utils/parse-int (filter #(not= "x" %) (str/split bus-defs #",")))]
      (apply * (min-with
                second
                (map (fn [bus]
                       [bus (- bus
                               (mod earliest-departure-time bus))])
                     buses))))))

(defn success-condition [candidate buses]
  (every?
   #(= 0
       (mod (+ (first %) candidate) (second %)))
   buses))

(defn solve-2 []
  (with-open [rdr (utils/input-reader filename)]
    (let [input (line-seq rdr)
          bus-defs (second input)
          buses (remove
                 #(= "x" (second %))
                 (map-indexed (fn [i elm]
                                [i (utils/parse-int elm)]) (str/split bus-defs #",")))
          step-size (second (max-with second buses))
          candidates (range step-size Integer/MAX_VALUE step-size)]
      (utils/first-where
       #(success-condition % buses)
       candidates))))

(comment
  (def filename "939
7,13,x,x,59,x,31,19")
  nil)
