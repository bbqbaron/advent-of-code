(ns aoc.day22
  "I looked up what to do, so this one doesn't count. Utterly pointless."
  (:require [clojure.string :as str]
            [aoc.prelude :refer [pt cp mp ap mv fs sd rd sunder ud fl]]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]))

(defn overlap? [v v2]
  (let [[[l r] [l2 r2]] (sort-by first [v v2])]
    (when (>= r l2)
      [(max l l2) (min r r2)])))

(defn overlap-cs? [c c2]
  (let [overlaps (vec (filter some? (map #(ap overlap? (map (fn [c] (nth c %))
                                                            [c c2]))
                                         (range 3))))]
    (when (= 3 (count overlaps))
      overlaps)))

(def small-sample
  "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10")

(def large-sample
  "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682")

(defn parse [s]
  (mp
   (cp
    (fn [[on? coords]]
      [(= on? "on")
       (mapv
        (cp
         (fn [[a b c]] [(edn/read-string b)
                        (edn/read-string c)])
         rest
         (pt re-find #"([xyz])=(-?\d+)\.\.(-?\d+)"))
        coords)])
    (juxt second (pt take-last 3))
    (pt re-find #"((on)|(off)) (x=.+)(y=.+)(z=.+)"))
   (str/split-lines s)))

(defn ct-cu [c]
  (rd * (mp (cp inc - (pt ap -)) c)))

(defn useless-transcription [cuboids]  
  (fs
   (rd
    (fn [[sc cs] [on? c]]
      [(if on?
         (let [ols (fl (cp some? sd) (mp #(update % 1 (pt overlap-cs? c)) cs))]
           (+ sc (- (ct-cu c) (useless-transcription ols))))
         sc)
       (cons [true c] cs)])
    [0 nil]
    cuboids)))

(useless-transcription
 (fl
  (cp (pt every? (pt every? (cp (pt >= 50) #(Math/abs %)))) sd)
  (reverse (parse large-sample))))

(useless-transcription
 (reverse (parse large-sample)))


(useless-transcription
 (reverse (parse (slurp (io/resource "22bigsample")))))
(useless-transcription
 (reverse (parse (slurp (io/resource "22")))))
