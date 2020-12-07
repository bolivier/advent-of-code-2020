(ns aoc.utils
  (:require [clojure.java.io :as io]))

(defn input-reader
  "Returns a `java.io.BufferedReader` of the contents.

  Typically you're going to call `line-seq` with this."
  [filename]
  (io/reader (str "resources/" filename)))

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


(comment
  (def filename "day-6.input")
  (def input (input-reader "day-5.input"))
  nil)
