(ns aoc.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn to-victory-paths
  "I hate managing double arrays, so stop doing it ASAP"
  [board-vec]
  (map
   (partial map set)
   [board-vec
    (apply map vector board-vec)]))

(defn parse-board [lns]
  (to-victory-paths
   (mapv
    (comp
     (partial mapv
              #(Integer/parseInt %))
     (partial remove str/blank?)
     #(str/split % #"\s+"))
    lns)))

(defn input [file]
  (let [[draw _ & boards]
        (str/split-lines
         (slurp (io/resource file)))]
    [(map read-string (str/split draw #","))
     (map parse-board
          (partition-all 5 (remove str/blank? boards)))]))

(defn score [picked? [rows cols]]
  (when (some (partial some #(set/subset? % picked?)) [rows cols])
    (reduce +
            (mapcat
             (partial remove picked?)
             rows))))

(defn step [pathsets picked? n]
  (for [[id paths] pathsets
        :let [score (some->> paths (score (conj picked? n)) (* n))]
        :when (and score (pos? score))]
    [id score]))

(defn run-picks [file]
  (let [[picks og-pathsets] (input file)]
    (reductions
     (fn [{:keys [picked? winners pathsets]} p]
       (let [new-winners (step pathsets picked? p)]
         {:picked? (conj picked? p)
          :winners (into winners (map second new-winners))
          :pathsets (apply dissoc pathsets
                           (map first new-winners))}))
     {:picked? #{} :winners [] :pathsets (into {} (map-indexed vector og-pathsets))}
     picks)))

(defn part1 []
  (->> (run-picks "4.txt")
       (some (comp seq :winners))
       first))

(defn part2 []
  (->> (run-picks "4.txt")
       last
       :winners
       last))

(comment
  (part1)
  (part2))
