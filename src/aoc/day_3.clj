(ns aoc.day-3
  (:require [clojure.string :as str]))

(def width 31)

(defn plot-route
  ([toboggan-map] (plot-route toboggan-map 1 3))
  ([toboggan-map dy dx]
   (let [width (count (first toboggan-map))]
     (loop [x 0
            y 0
            route []]
       (if (<= (count toboggan-map) y)
         route
         (recur (+ dx x)
                (+ dy y)
                (conj route (get-in toboggan-map [y (mod x width)]))))))))

(defn solve []
  (let [input (slurp "resources/day-3.input")
        toboggan-map (str/split input #"\n")]
    (-> toboggan-map
        plot-route
        frequencies
        (get \#))))

(defn solve-2 []
  (let [input (slurp "resources/day-3.input")
        toboggan-map (str/split input #"\n")
        map-plotter (partial plot-route toboggan-map)
        slope-vals [[1 1] [1 3] [1 5] [1 7] [2 1]]]
    (apply *
           (for [slope-val slope-vals]
             (get (frequencies (apply map-plotter slope-val)) \#)))))

(comment
  (def input "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")
  (def toboggan-map (str/split input #"\n"))
  (def width (count (first toboggan-map)))

  nil)
