(ns aoc.day-14
  (:require [clojure.string :as str]
            [aoc.utils :as utils]))

(def initial-state {:memory {}
                    :mask nil})

(defn left-pad [s n]
  (str (str/join "" (repeat (- n (count s)) "0"))
       s))

(defn int->boolean-string [n]
  (left-pad (Integer/toString n 2)
            36))

(defn boolean-string->int [s]
  (BigInteger. s 2))

(defn apply-mask [mask n]
  (str/join ""
        (mapv
         (fn [[mask-elm n-elm]]
           (if (= \X mask-elm)
             n-elm
             mask-elm))
         (map vector mask (int->boolean-string n)))))

(defn step [state instruction]
  (if (str/starts-with? instruction "mask =")
    (assoc state :mask (last (str/split instruction #" ")))
    (update state :memory (fn [memory]
                            (let [mem-location (second (re-matches #".*\[(\d+)\].*" instruction))
                                  mem-value (second (re-matches #".*= (\d+)" instruction))]
                              (assoc memory mem-location (boolean-string->int
                                                          (apply-mask (:mask state) (utils/parse-int mem-value)))))))))

(defn solve []
  (reduce +
          0
          (vals (:memory
                 (reduce
                  step
                  initial-state
                  (str/split-lines (slurp "resources/day-14.input")))))))


(comment
  (do
    (def n 5)))
