(ns aoc.day-4
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [instaparse.core :as insta]))

(defn parse-int [^String s]
  (try
    (Integer/parseInt s)
    (catch java.lang.NumberFormatException _
      s)))

(s/def ::byr int?)
(s/def ::iyr int?)
(s/def ::eyr int?)
(s/def ::ecl string?)
(s/def ::pid string?)
(s/def ::hcl string?)
(s/def ::cid string?)
(s/def ::hgt string?)
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
    raw-vals))

(defn parse-input [input]
  (map parse-single-input
       (str/split input #"\n\n")))

(defn solve []
  (let [raw-input (slurp "resources/day-4.input")
        input (parse-input raw-input)]
    (count (filter #(= required-fields
                       (set/intersection required-fields
                                         (into #{} (keys %))))
                   input))))

{:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"}

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
