(ns aoc.day-15)

(def input [18 11 9 0 5 1])

(defn initial-state [input]
  (reduce
   #(assoc %1 %2 (list (count %1)))
   {}
   input))

(defn cons-coll [coll r]
  (reduce
   (fn [acc x]
     (cons x
            acc))
   r
   (reverse coll)))

(defn solution-seq
  ([input]
   (cons-coll input
              (lazy-seq
               (solution-seq (count input)
                             (last input)
                             (initial-state input)))))
  ([index last-val state]
   (let [val (if (< 1 (count (state last-val)))
               (apply - (take 2 (state last-val)))
               0)]
     (lazy-seq (cons val
                     (solution-seq (inc index)
                                   val
                                   (update state val #(conj % index))))))))

(defn solve []
  (let [idx (dec 2020)]
    (nth (solution-seq input)
         idx)))

(defn solve-2 []
  (let [idx (dec 30000000)]
    (nth (solution-seq input)
         idx)))


(comment
  (def input [0 3 6])
  nil)
