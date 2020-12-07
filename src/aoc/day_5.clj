(ns aoc.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn first-half [coll]
  (let [half (int (/ (count coll)
                     2))]
    (take half coll)))

(defn second-half [coll]
  (let [half (int (/ (count coll)
                     2))]
    (drop half coll)))

(defn get-seat [boarding-pass]
  (loop [possible-rows (range 128)
         possible-columns (range 8)
         bp boarding-pass]
    (case (first bp)
      \F (recur (first-half possible-rows)
                possible-columns
                (rest bp))
      \B (recur (second-half possible-rows)
                possible-columns
                (rest bp))
      \L (recur possible-rows
                (first-half possible-columns)
                (rest bp))
      \R (recur possible-rows
                (second-half possible-columns)
                (rest bp))
      nil [(first possible-rows) (first possible-columns)])))

(defn get-id [[row col]]
  (+ (* 8 row)
     col))

(defn solve []
  (let [lines (->> "day-5.input"
                   io/resource
                   slurp
                   str/split-lines)]
    (apply max
           (map
            (comp get-id get-seat)
            lines))))

(defn solve-2 []
  (let [lines (->> "day-5.input"
                   io/resource
                   slurp
                   str/split-lines)
        adjacent? (fn [[f s]]
                    (= 1 (- s f )))]
    (->> lines
         (map (comp get-id get-seat))
         sort
         (partition 2 1)
         (remove adjacent?)
         ffirst
         inc)))
