(ns aoc.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn input [file]
  (map
   clojure.edn/read-string
   (-> file
       io/resource
       slurp
       str/split-lines)))

;; naive approach; how far can we get?

(defn positions->costs [distance->cost ps]
  (let [[left right] ((juxt first last) (sort ps))
        freqs (frequencies ps)]
    (map
     (fn [point]
       [point (map
           (fn [[point2 n]]
             (let [distance (Math/abs (- point point2))]
               [point2 (* n (distance->cost distance))]))
           freqs)])
     (range left (inc right)))))

(defn node-cost [cs] (reduce + (map second cs)))

(defn costs->winner [costs]
  (second
   (first
    (sort-by
     second
     (map
      #(update % 1 node-cost) 
      costs)))))

(defn part1 [file]
  (costs->winner (positions->costs identity (input file))))

(defn part2 [file]
  (costs->winner (positions->costs #(* % (inc %) 0.5) (input file))))

(comment
  (part1 "7sample.txt")
  (part2 "7sample.txt")
  ;; this runs for like 10 seconds; ehhhhhhhhhh
  (part1 "7.txt")
  (part2 "7.txt")
  )
