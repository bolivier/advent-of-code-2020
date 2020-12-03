(ns aoc.day-2
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]))

(defn parse-input-line [line]
  (let [[rule password] (map str/trim (str/split line #":"))
        [counts character] (str/split rule #" ")
        c (-> character char-array first)
        limits (mapv #(Integer/parseInt %) (str/split counts #"-"))]
    [limits c password]))

(defn parse-input [input]
  (map
   parse-input-line
   (str/split input #"\n")))

(s/def ::parsed-input
  (s/cat :limits (s/coll-of int?)
         :char char?
         :password string?))

(defn validate-input-line [[limits c password]]
  (let [[lower upper] limits
        char-count (get (frequencies password) c 0)]
    (<= lower char-count upper)))

(defn solve []
  (->> "resources/day-2.input"
       slurp
       parse-input
       (filter validate-input-line)
       count))

(defn xor [a b]
  (and (or a b)
       (not (and a b))))

(defn validate-input-line-2 [[limits c password]]
  (let [[char-1 char-2] (mapv #(get password (dec %)) limits)]
    (xor (= c char-1)
         (= c char-2))))

(defn solve-2
  "The input validation is different here.  `1-3` refers not to counts
  of 1 through 3, but positions 1 or 3, where the described letter
  must exist."
  []
  (let [input (slurp "resources/day-2.input")
        values (parse-input input)]
    (count (filter validate-input-line-2 values))))

(comment

  (def line "1-3 a: abcde")
  (def line "1-3 b: cdefg")
  (def input "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

  ;; for validation
  (do
    (def limits [1 3])
    (def c \b)
    (def password "abcde"))

  nil)
