d(ns aoc.day-11
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn parse [rdr]
  (mapv #(str/split % #"")
        (line-seq rdr)))

(defn direction-seq [board x y dy dx]
  (let [yp (+ y dy)
        xp (+ x dx)]
   (lazy-seq (cons (get-in board [yp xp])
                   (direction-seq board
                                  xp
                                  yp
                                  dy
                                  dx)))))

(def floor-char ".")
(def empty-char "L")
(def occupied-char "#")

(defn occupied-seat? [val]
  (= occupied-char val))

(defn empty-seat? [val]
  (= empty-char val))

(defn seat? [val]
  (or (occupied-seat? val)
      (empty-seat? val)))

(defn first-where [pred coll]
  (first (drop-while (comp not pred) coll)))

(def directions
  (for [dx (range -1 2)
        dy (range -1 2)
        :when (not (and (zero? dx)
                        (zero? dy)))]
    [dy dx]))

(defn get-adjacent [board x y]
  (map
   (fn [[dy dx]]
     (first-where
      (some-fn nil? seat?)
      (direction-seq board x y
                     dy dx)))
   directions))

(defn get-next-val [elm adjacents]
  (let [occupied-adjacents (count (filter occupied-seat? adjacents))]
    (cond
      (= "." elm)
      elm

      (and (= 0 occupied-adjacents)
           (empty-seat? elm))
      "#"

      (and (<= 4 occupied-adjacents)
           (occupied-seat? elm))
      "L"

      :else elm)))

(defn step [board]
  (into []
        (map-indexed
         (fn [y row]
           (into []
                 (map-indexed
                  (fn [x elm]
                    (get-next-val elm (get-adjacent board x y)))
                  row)))
         board)))

(defn step-seq [board]
  (let [next-board (step board)]
    (if (= next-board board)
      nil
      (lazy-seq (cons next-board (step-seq next-board))))))

(defn serialize [board]
  (str/join "\n" (map #(str/join "" %) board)))

(def filename "day-11.input")

(defn solve []
  (count
   (filter
    occupied-seat?
    (flatten
     (with-open [rdr (utils/input-reader filename)]
       (let [board (parse rdr)]
         (last (take-while (comp not nil?) (step-seq board)))))))))

(defn solve-2 []
  (with-open [rdr (utils/input-reader empty-state)]
    (let [board (parse rdr)]
      (last (take-while (comp not nil?) (step-seq board))))))

(comment
  (def empty-state "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")
  (def first-iteration
    "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##")

  (def board (mapv #(str/split % #"")(line-seq (utils/input-reader first-iteration))))
  board
  nil)
