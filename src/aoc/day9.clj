(ns aoc.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(defn- input [file]
  (->> file
       io/resource
       slurp
       str/split-lines
       (mapv (comp (partial mapv clojure.edn/read-string) #(str/split % #"")))))

(defn get-neighbors [my mx y x]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [x2 (+ x dx)
              y2 (+ y dy)]
        :when (<= 0 x2 mx)
        :when (<= 0 y2 my)
        :when (some (complement zero?) [dx dy])
        :when (some zero? [dx dy])]
    [y2 x2]))

(defn local-minima [grid]
  (let [mx (dec (count (first grid)))
        my (dec (count grid))]
    (filter some?
            (reduce concat
                    (map-indexed
                     (fn [y row]
                       (map-indexed
                        (fn [x me]
                          (let [neighbors (map (partial get-in grid) (get-neighbors my mx y x))]
                            (when (every? #(> % me) neighbors)
                              [[y x] me])))
                        row))
                     grid)))))

(defn basins [grid]
  (let [lms (local-minima grid)
        mx (dec (count (first grid)))
        my (dec (count grid))]
    (map
     (partial 
      (fn go [seen [[y x] lm]]
        (case lm 9 #{}
              (let [neighbor-pts (remove seen (get-neighbors my mx y x))
                    neighbors (filter
                               (comp #(> 9 % lm) second)
                               (map (juxt identity (partial get-in grid))
                                    neighbor-pts))]
                (if (seq neighbors)
                  (conj
                   (reduce set/union
                           (map
                            (partial go (conj seen [y x]))
                            neighbors))
                   [y x])
                  #{[y x]}))))
      #{})
     lms)))

(comment
  (reduce + (map (comp inc second) (local-minima (input "9.txt"))))
  (reduce *
          (take-last
           3
           (sort
            (map count (basins (input "9.txt")))))))
