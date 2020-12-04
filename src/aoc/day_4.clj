(ns aoc.day-4
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defn update-when [coll k f]
  (if (get coll k)
    (update coll k f)
    coll))

(defn parse-int [^String s]
  (try
    (Integer/parseInt s)
    (catch java.lang.NumberFormatException _
      s)))
(s/def ::byr (s/and int?
                    #(<= 1920 % 2002)))
(s/def ::iyr (s/and int?
                    #(<= 2010 % 2020)))
(s/def ::eyr (s/and int?
                    #(<= 2020 % 2030)))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid (s/and #(= 9 (count %))
                    #(boolean (re-matches #"\d{9}" %))))
(s/def ::hcl #(boolean (re-matches #"#[a-f0-9]{6}" %)))
(s/def ::hgt
  (s/and string?
         (let [extract-n #(->> %
                               (drop-last 2)
                               (str/join "")
                               parse-int)]
           (s/or :inches
                 (s/and #(str/ends-with? % "in")
                        #(<= 59 (extract-n %) 76))
                 :cm (s/and #(str/ends-with? % "cm")
                            #(<= 150 (extract-n %) 193))))))
(s/def ::cid string?)
(s/def ::passport (s/keys :req-un [::ecl
                                   ::pid
                                   ::eyr
                                   ::hcl
                                   ::byr
                                   ::iyr
                                   ::hgt]
                          :opt-un [::cid]))
(def required-fields #{:ecl :byr :iyr :hgt :pid :hcl :eyr})

(defn parse-single-input [single]
  (let [elements  (str/split single #"\s")
        parse-single (fn [elm]
                       (let [[k v] (str/split elm  #":")]
                         [(keyword k) v]))
        raw-vals (into {} (map parse-single elements))]
    (-> raw-vals
        (update-when :byr parse-int)
        (update-when :iyr parse-int)
        (update-when :eyr parse-int))))

(defn parse-input [input]
  (map parse-single-input
       (str/split input #"\n\n")))

(defn solve []
  (let [raw-input (slurp "resources/day-4.input")
        input (parse-input raw-input)]
    (->> input
         (map #(into #{} (keys %)))
         (map (partial set/intersection required-fields))
         (filter #(= required-fields %))
         count)))

(defn solve-2 []
  (let [raw-input (slurp "resources/day-4.input")
        input (parse-input raw-input)]
    (->> input
         (filter #(s/valid? ::passport %))
         count)))

(comment
  (def raw-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")
  nil)
