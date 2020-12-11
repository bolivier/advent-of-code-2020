(ns aoc.day-10
  (:require [aoc.utils :as utils]))

(defn all-adapters [my-adapters]
  (let [outlet 0
        device-adapter (+ 3 (apply max my-adapters))]
    (conj my-adapters outlet device-adapter)))

(defn jolt-difference-frequencies [adapters-list]
  (frequencies (map #(- (second %) (first %))
                    (partition 2 1 (sort adapters-list)))))

(def filename "day-10.input")

(defn solve []
  (let [input (map utils/parse-int  (line-seq (utils/input-reader filename)))
        adapters (all-adapters input)
        freqs (jolt-difference-frequencies adapters)]

    (* (freqs 3)
       (freqs 1))))

(defn exp [b n]
  (reduce * (bigint 1) (repeat n b)))

(let [input (map utils/parse-int (line-seq (utils/input-reader filename)))]
  (exp 2 (reduce +
                 (for [size (range 2 5)]
                   (count (remove #(< 3 %) (map #(- (last %)
                                                    (first %)) (partition size 1 (sort (conj input 0))))))))))

(comment
  (def filename "16
10
15
5
1
11
7
19
6
12
4")
  nil)
