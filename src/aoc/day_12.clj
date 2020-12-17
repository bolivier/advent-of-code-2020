(ns aoc.day-12
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def initial-state {:direction "E"
                    :x 0
                    :y 0
                    :waypoint {:x 10
                               :y 1}})

(defn parse-instruction [line]
  {:direction (str (first line))
   :unit (utils/parse-int (str/join "" (rest line)))})

(defn invert-direction [direction]
  (case direction
    "E" "W"
    "W" "E"
    "N" "S"
    "S" "N"))

(defmulti move (fn [_ instruction]
                 (-> instruction :direction)))

(defmethod move "N" [state {:keys [unit]}]
  (update-in state [:waypoint :y] #(+ % unit)))

(defmethod move "S" [state {:keys [unit]}]
  (update-in state [:waypoint :y] #(- % unit)))

(defmethod move "E" [state {:keys [unit]}]
  (update-in state [:waypoint :x] #(+ % unit)))

(defmethod move "W" [state {:keys [unit]}]
  (update-in state [:waypoint :x] #(- % unit)))

(defmethod move "F" [state {:keys [unit]}]
  (-> state
      (update-in state))
  (move state {:direction (:direction state)
               :unit unit}))

(defmethod move "B" [state {:keys [unit]}]
  (move state {:direction (invert-direction (:direction state))
               :unit unit}))

(def directions ["E" "S" "W" "N"])
(defmethod move "R" [state {:keys [unit]}]
  (update state :direction
          (fn [direction]
            (nth (drop-while #(not= direction %) (cycle directions))
                 (/ unit 90)))))

(defmethod move "L" [state {:keys [unit]}]
  (update state :direction
          (fn [direction]
            (nth (drop-while #(not= direction %) (cycle (reverse directions)))
                 (/ unit 90)))))

(defn manhattan-distance [{:keys [x y]}]
  (+ (Math/abs x)
     (Math/abs y)))

(def filename "day-12.input")

(defn solve []
  (->> filename
       utils/input-reader
       line-seq
       (map parse-instruction)
       (reduce move initial-state)
       manhattan-distance))




(comment

  (def filename "F10
N3
F7
R90
F11")

  (def all-turns-right-angles? (=
                                0
                                (transduce
                                 (comp (map parse-instruction)
                                       (filter #(contains? #{"L" "R"} %))
                                       (map :unit)
                                       (map #(mod % 90)))
                                 +
                                 (line-seq (utils/input-reader "day-12.input")))))
  (transduce
   (comp (map parse-instruction)
         (filter #(contains? #{"L" "R"} (:direction %)))
         (map :unit))
   conj
   #{}
   (line-seq (utils/input-reader "day-12.input")))
  ;;;  => true
  nil)
