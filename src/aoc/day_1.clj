(ns aoc.day-1
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn read-input []
  (mapv
   #(Integer/parseInt %)
   (str/split (slurp "resources/day-1.input") #"\n")))

(defn sum-set [expenses sum]
  (set/intersection
           (into #{} expenses)
           (into #{} (map #(- sum %) expenses))))

(defn multiply-expenses
  ([expenses] (multiply-expenses expenses 2020))
  ([expenses sum]
   (apply *
          (set/intersection
           (into #{} expenses)
           (into #{} (map #(- sum %) expenses))))))

(defn triple-sum-set
  "Calculate the sum set for a given sum, the calculate the original
  value using 2020 as an assumed original sum for this triple."
  [expenses sum]
  (let [inverse-of-sum (* -1
                          (- sum 2020))]
    (conj (sum-set expenses sum) inverse-of-sum)))

(defn multiply-2020-expenses-2 [expenses]
  (->> expenses
       (mapv #(- 2020 %))
       (mapv (partial triple-sum-set expenses))
       (remove #(= 1 (count %)))
       first
       (apply *)))

(defn solve []
  (let [input (read-input)]
    (multiply-expenses input)))

(defn solve-2-mine []
  (let [input (read-input)]
    (multiply-2020-expenses-2 input)))

(defn solve-2 []
  (let [input (read-input)]
    (first (for [x     input
                 y     input
                 z     input
                 :when (= 2020 (+ x y z))]
             (* x y z)))))

(clojure.core/time (dotimes [_ 100]
                     (solve-2)))
(clojure.core/time (dotimes [_ 100]
                     (solve-2-mine)))





(comment
  (def expenses [1721
                 979
                 366
                 299
                 675
                 1456])


  nil)
