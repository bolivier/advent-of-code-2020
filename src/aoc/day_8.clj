(ns aoc.day-8
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def filename "day-8.input")

(defn natural-numbers
  ([] (natural-numbers 1))
  ([n] (lazy-seq (cons n (natural-numbers (inc n))))))

(take 10 (natural-numbers))

(defn get-instructions []
  (let [lines (str/split-lines (slurp (utils/input-reader filename)))]
    (mapv
     #(let [[instruction arg] (str/split % #" ")]
        {:instruction instruction
         :arg (Integer/parseInt arg)})
     lines)))

(defn instruction-seq
  ([instructions] (lazy-seq (cons 0 (instruction-seq
                                     instructions 0))))
  ([instructions pc]
   (let [new-pc  (if (or (nil? pc)
                         (<= (count instructions) pc))
                   nil
                   (case (:instruction (nth instructions pc))
                     "nop" (inc pc)
                     "acc" (inc pc)
                     "jmp" (+ pc (:arg (nth instructions pc)))))]
     (lazy-seq (cons new-pc (instruction-seq instructions new-pc))))))

(defn singly-executed-seq [coll]
  (let [executed (atom #{})]
    (take-while
     #(let [val (not (contains? @executed %))]
        (do (swap! executed conj %)
            val))
     (instruction-seq coll))))

(defn acc-change [instruction]
  (if (= "acc" (:instruction instruction))
    (:arg instruction)
    0))

(defn solve []
  (let [instructions (get-instructions)
        single-run-instructions (singly-executed-seq (instruction-seq instructions))]
    (transduce
     (comp (map #(nth instructions %))
           (filter #(= (:instruction %) "acc"))
           (map :arg))
     +
     0
     single-run-instructions)))

(defn copy [coll n]
  (mapv
   (constantly coll)
   (range n)))

(defn updatev [v idx f]
  (let [up-to-idx (conj
                   (subvec v 0 idx)
                   (f (nth v idx)))]
    (try
      (apply conj up-to-idx (subvec v (inc idx)))
      (catch IndexOutOfBoundsException _
        up-to-idx))))

(defn instruction-iterations [instructions]
  (let [iterations (copy instructions (count instructions))]
    (map-indexed
     (fn [idx iteration]
       (updatev iteration idx (fn [{:keys [instruction]
                                    :as inst}]
                                (case instruction
                                  "jmp" (assoc inst :instruction "nop")
                                  "nop" (assoc inst :instruction "jmp")
                                  inst))))
     iterations)))

(defn solve-2 []
  (let [instructions (get-instructions)
        iterations (instruction-iterations instructions)
        interesting-iterations (remove
                                #(= instructions %)
                                iterations)

        ending-seq (first (filter
                           #(nil? (last (singly-executed-seq %)))
                           interesting-iterations))]
    (->> ending-seq
         singly-executed-seq
         (drop-last 2) ;; final nil value
         (map #(nth ending-seq %))
         (filter #(= "acc" (:instruction %)))
         (map :arg)
         (reduce +))))

(comment

  (let [instructions (get-instructions)
        iterations (instruction-iterations instructions)
        interesting-iterations (remove
                                #(= instructions %)
                                iterations)

        ending-seq (first (filter
                           #(nil? (last (singly-executed-seq %)))
                           interesting-iterations))]
    (->> ending-seq
         singly-executed-seq
         (drop-last 2) ;; final nil value
         (map #(nth ending-seq %))
         (filter #(= "acc" (:instruction %)))
         (map :arg)
         (reduce +)))

  nil)

(comment
  (def filename "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
")

  (line-seq (utils/input-reader program))
  g
  nil)
