(ns aoc.day20
  (:require
   [aoc.coords :refer [ns3x3]]
   [aoc.prelude :refer :all]
   [clojure.set :as set]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.edn :as edn]))

(def sample-algo
  (str/replace
   "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
   #"\n" ""))

(def sample-img
  (mapv
   vec
   (str/split-lines
    "#..#.
#....
##..#
..#..
..###")))

(defn pxl-pos [default img y x]
  (bin->dec
   (map
    #(case % \. 0 \# 1)
    (map
     #(or % default)
     (map (pt get-in img) (ns3x3 y x))))))

(defn decode [default algo img y x]
  (nth algo
       (pxl-pos default img y x)))

(defn do-img [default algo img n]
  (if (<= n 0)
    img
    (do-img
     (nth algo
          (bin->dec
           (map
            #(case % \. 0 \# 1)
            (mapcat (pt repeat 3)
                    (repeat 3 default)))))
     algo
     (mapv vec
           (partition
            (+ 2 (count img))
            (for [y (range -1 (inc (count img)))
                  x (range -1 (inc (count (first img))))]
              (decode default algo img y x))))
     (dec n))))

(assert
 (=  35
     (count (filter #{\#} (reduce concat (do-img
                                          \.
                                          sample-algo sample-img 2))))))
 (=  3351
     (count (filter #{\#} (reduce concat (do-img
                                          \.
                                          sample-algo sample-img 50)))))
(def input (str/split
            (slurp (io/resource "20"))
            #"\n\n"))
(def algo (first input))
(def img (mapv vec (str/split-lines (second input))))

(count
 (filter
  #{\#}
  (reduce concat
          (do-img
           \.
           algo img
           2))))

 (count
  (filter
   #{\#}
   (reduce concat
           (do-img
            \.
            algo img
            50))))
