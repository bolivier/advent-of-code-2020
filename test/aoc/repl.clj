(ns aoc.repl
  (:require [kaocha.watch :as kw]
            [kaocha.repl :as k]))

(defn watch-tests []
  (kw/run (k/config)))
