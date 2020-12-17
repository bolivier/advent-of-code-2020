(ns aoc.utils
  (:require [clojure.java.io :as io]))

(defn input-reader
  "Returns a `java.io.BufferedReader` of the contents.

  Typically you're going to call `line-seq` with this."
  [filename]
  (try
    (io/reader (str "resources/" filename))
    (catch java.io.FileNotFoundException _
      (-> filename
          (java.io.StringReader.)
          (java.io.BufferedReader.)))))

(def blank-line-splitting
  (comp (partition-by empty?)
        (remove #(and (= 1 (count %))
                      (empty? (first %))))))

(defn partition-by-blank-lines
  "Split input from seq into a vector of vectors where each subvector is
  newline separated and each larger vector is blank line separated.

  abc
  bc

  adc
  dc

  becomes

  [[\"abc\" \"bc\"]
  [\"adc\" \"dc\"]]"
  [input]
  (transduce blank-line-splitting
             conj
             []
             input))

(defn parse-int [^String s]
  (try
    (Integer/parseInt s)
    (catch java.lang.NumberFormatException _
      s)))

(defn first-where [pred coll]
  (first (drop-while (comp not pred) coll)))

(comment
  (def filename "day-6.input")
  (def input (input-reader "day-5.input"))
  nil)
