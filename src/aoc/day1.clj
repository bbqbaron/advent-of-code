(ns aoc.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def measurements
  (map
   #(Integer/parseInt %)
   (str/split-lines
    (slurp (io/resource "1.txt")))))

(defn day1-part1 []
  (->> measurements
       rest
       (map vector
            measurements)
       (filter
        (partial apply <))
       count))

(defn day1-part2 []
  (let [triples (map
                 +
                 measurements
                 (rest measurements)
                 (drop 2 measurements))]
    (->> triples
         rest
         (map vector
              triples)
         (filter
          (partial apply <))
         count)))
