(ns aoc.day-9
  (:require [aoc.utils :as utils]))

(defn two-sum [coll n]
  (let [inverses (frequencies (map #(- n %) coll))]
    (some
     #(and (contains? inverses %)
           (if (= (/ n 2)
                  %)
             (<= 2 (inverses %))
             true))
     coll)))

(def filename "day-9.input")
(def preamble-size 25)

(defn solve []
  (let [lines (line-seq (utils/input-reader filename))]
    (->> lines
         (map utils/parse-int)
         (partition (inc preamble-size) 1)
         (map (fn [line]
                [(drop-last line) (last line)]))
         (drop-while #(two-sum (first %) (second %)))
         first
         last)))

(def invalid-number 29221323)

(defn solve-2-broken []
  (loop [remaining-lines (map utils/parse-int (line-seq (utils/input-reader filename)))]
    (let [sum (atom 0)
          total-seq (take-while #(let [new-total (+ % @sum)]
                                   (when (<= new-total invalid-number)
                                     (reset! sum new-total)
                                     true))
                                remaining-lines)]
      (cond
        (= invalid-number @sum) (+ (apply min total-seq)
                                   (apply max total-seq))
        (empty? remaining-lines) nil
        :else (recur (rest remaining-lines))))))

(defn solve-2 []
  (let [invalid-num (solve)]
    (loop [remaining-lines (map utils/parse-int (line-seq (utils/input-reader filename)))]
      (let [sum (atom 0)
            total-seq (doall
                       (take-while #(let [new-total (+ % @sum)]
                                      (when (<= new-total invalid-num)
                                        (reset! sum new-total)
                                        true))
                                   remaining-lines))]
        (cond
          (= invalid-num @sum) (+ (apply min total-seq)
                                  (apply max total-seq))
          (empty? remaining-lines) nil
          :else (recur (rest remaining-lines)))))))

(defn solve-3 []
  (loop [remaining-lines (map utils/parse-int (line-seq (utils/input-reader filename)))]
    (let [sum (atom 0)
          total-seq (take-while #(do
                                   (swap! sum (partial + %))
                                   (<= @sum invalid-number))
                                remaining-lines)]
      (cond
        (= invalid-number (reduce + total-seq)) (+ (apply min total-seq)
                                                   (apply max total-seq))
        (empty? remaining-lines) nil
        :else (recur (rest remaining-lines))))))

(comment
  (two-sum [2 5 8 10] 7)
  (two-sum [2] 4)

  (def preamble-size 5)
  (def invalid-number 127)

  (def filename "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")
  nil)
