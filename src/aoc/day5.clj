(ns aoc.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn read-dimensions [line]
  (->> (str/split line #" -> ")
       (map
        (comp
         (partial map edn/read-string)
         #(str/split % #",")))
       (apply map vector)))

(defn pt-range [[l r]]
  (let [desc? (> l r)]
    (range l
           (cond-> r desc? dec (<= l r) inc)
           (if desc? -1 1))))

(defn dims->pts
  [dims]
  (take
   (apply max (map (comp inc #(Math/abs %) (partial apply -)) dims))
   (apply map vector
          (map (comp
                (partial apply concat)
                repeat
                pt-range)
               dims))))

(defn diagonal? [dims]
  (every? (comp (partial < 1) count set) dims))

(defn dims->cts [pts]
  (->> pts
       (mapcat dims->pts)
       frequencies
       (filter (comp (partial < 1) second))
       count))

(defn input [file]
  (->> file
       io/resource
       slurp
       str/split-lines
       (map
        read-dimensions)))

(defn part1 []
  (->> (input "5.txt")
       (remove diagonal?)
       (dims->cts)))

(defn part2 []
  (dims->cts (input "5.txt")))

(comment
  (part1)
  (part2)
  )
