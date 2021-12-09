(ns aoc.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def segments
  (into {} (map-indexed vector [#{:a :b :c :e :f :g}
                                #{:c :f}
                                #{:a :c :d :e :g}
                                #{:a :c :d :f :g}
                                #{:b :c :d :f}
                                #{:a :b :d :f :g}
                                #{:a :b :d :e :f :g}
                                #{:a :c :f}
                                #{:a :b :c :d :e :f :g}
                                #{:a :b :c :d :f :g}])))

(defn- input [file]
  (->> file
       io/resource
       slurp
       str/split-lines
       (map (comp
             (partial map
                      (comp
                       (partial map (comp set (partial map (comp keyword str))))
                       (partial remove str/blank?)
                       #(str/split % #"\s+")))
             #(str/split % #"\|")))))

(defn- encode
  [s] (ffirst (filter (comp #{s} second) segments)))

(comment
  (reduce + (for [[clues numbers] (input "8.txt")
                  :let [by-length (group-by count clues)
                        [cf seven four] (map first (map by-length (range 2 5)))
                        a (set/difference seven cf)
                        bd (set/difference four cf)
                        fives (reduce set/intersection (by-length 5))
                        g (set/difference fives a bd)
                        c (set/difference cf (reduce set/intersection (by-length 6)))
                        f (set/difference cf c)
                        d (set/difference fives a g)
                        b (set/difference bd d)
                        e (set/difference (first (by-length 7)) a b c d f g)
                        mapping (into {}
                                      (map #(update % 0 first)
                                           {a :a b :b c :c d :d e :e f :f g :g}))]]
              (clojure.edn/read-string (apply str (drop-while zero? (map
                                                                     (comp encode set (partial map mapping))
                                                                     numbers)))))))
