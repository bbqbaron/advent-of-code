(ns aoc.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (map
   (fn [line]
     (rest
      (update (re-find #"(\w+) (\d+)" line)
              2
              #(Integer/parseInt %))))
   (str/split-lines
    (slurp (io/resource "2.txt")))))

(defn part1 []
  (apply *
         (reduce
          (fn [[h v] [cmd amt]]
            (case cmd
              "forward" [(+ h amt) v]
              "up" [h (- v amt)]
              "down" [h (+ v amt)]))
          [0 0]
          input)))

(defn part2 []
  (apply *
         (take 2
               (reduce
                (fn [[h v a] [cmd amt]]
                  (case cmd
                    "forward" [(+ h amt) (+ v (* a amt)) a]
                    "up" [h v (- a amt)]
                    "down" [h v (+ a amt)]))
                [0 0 0]
                input))))
