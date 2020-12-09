(ns aoc.day-7
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :refer [union]]))

(def filename "day-7.input")

(defn to-contained-map [[raw-container raw-contained]]
  (let [container (keyword (str/join "-" (drop-last (str/split raw-container #" "))))]
    (->> #","
         (str/split   raw-contained)
         (map str/trim)
         (map (fn [s]
                (str/split s #" ")))
         (map (fn [[n & colors]]
                {(keyword (str/join "-" (drop-last colors))) #{container}})))))

(defn to-container-map [[raw-container raw-contained]]
  (let [container (keyword (str/join "-" (drop-last (str/split raw-container #" "))))
        containeds (map
                    (fn [[n & contained-info]]
                      (if (= "no" n)
                        nil
                        {:name (keyword (str/join "-" (drop-last contained-info)))
                         :n (Integer/parseInt n)}))
                    (map
                     #(str/split % #" ")
                     (str/split raw-contained #", ")))]
    {container (if (= containeds '(nil))
                 nil
                 containeds)}))

(def container-mapping (comp (map #(str/split % #" contain "))
                             (map to-container-map)
                             (map #(apply merge %))))

(def contained-mapping (comp (map #(str/split % #"contain"))
                             (map #(map str/trim %))
                             (map to-contained-map)
                             (map #(apply merge %))))

(defn solve []
  (let [m (transduce contained-mapping
                     (partial merge-with union)
                     (line-seq (utils/input-reader filename)))]
    (count (loop [bags-to-check [:shiny-gold]
                  bags-that-can-hold #{}]
             (if (empty? bags-to-check)
               bags-that-can-hold
               (let [bags (get m (first bags-to-check))]
                 (recur (apply conj (rest bags-to-check) bags)
                        (apply conj bags-that-can-hold bags))))))))

(defn count-bags [m bag-name]
  (if (= nil (m bag-name))
    0
    (apply +
           (conj
            (map
             :n
             (m bag-name))

            (apply +

                   (map (fn [{:keys [n name]}]
                          (* n (count-bags m name)))
                        (m bag-name)))))))

(defn part-2-map
  []
  (transduce container-mapping
             merge
             (line-seq (utils/input-reader filename))))

(defn solve-2 []
  (let [m (part-2-map)]
    (count-bags m :shiny-gold)))

(comment

  (def input
    "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

  (def input-2
    "faded blue bags contain no other bags.
dotted black bags contain no other bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.")

  (defn input-reader [_]
    (java.io.BufferedReader. (java.io.StringReader. input-2)))

  (def raw-contained "1 bright white bag, 2 muted yellow bags.")
  (def raw-container "light red bags")

  (def m (part-2-map))

  (def t [[:gold 1]
          [[:olive 1]
           [[:blue 3]]
           [[:black 4]]]
          [[:plum 3]
           [:blue 5]
           [:black 6]]])

  nil)
