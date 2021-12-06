(ns aoc.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn input [file]
  (mapv
   (comp
    #(or % 0)
    (->> file
         io/resource
         slurp
         str/split-lines
         (map #(str/split % #","))
         (map (partial map clojure.edn/read-string))
         first
         frequencies))
   (range 7)))

(defn step [[fs [hatchlings-1 hatchlings-2]] idx]
  (let [ready (fs idx)]
    [(-> fs
         (update idx + hatchlings-1))
     [hatchlings-2 ready]]))

(defn pass-days [fs] (reductions step [fs [0 0]] (cycle (range 7))))

(def count-fish (comp (partial reduce +) (partial apply concat)))

(defn part1 []
  (count-fish (nth (pass-days (input "6.txt")) 80)))

(defn part2 []
  (count-fish (nth (pass-days (input "6.txt")) 256)))

(comment
  (part1)
  (part2))
