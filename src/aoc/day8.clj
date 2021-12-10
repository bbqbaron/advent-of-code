(ns aoc.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def segments
  [#{:a :b :c :e :f :g}
   #{:c :f}
   #{:a :c :d :e :g}
   #{:a :c :d :f :g}
   #{:b :c :d :f}
   #{:a :b :d :f :g}
   #{:a :b :d :e :f :g}
   #{:a :c :f}
   #{:a :b :c :d :e :f :g}
   #{:a :b :c :d :f :g}])

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
  [s] (ffirst (filter (comp #{s} second) (map-indexed vector segments))))

(defn by-counts [signals]
  (->> signals
       (group-by count)
       (map #(update % 1 (partial reduce set/intersection)))
       (mapcat (fn [[idx xs]] (map (partial vector idx) xs)))
       (reduce (fn [acc [idx x]] (update acc x (fnil conj #{}) idx)) {})       
       (map (comp vec reverse))
       (into {})))

(def segment-counts (by-counts segments))

(defn to-canonical [[clues numbers]]
  (let [mapping (as-> clues $
                  (by-counts $)
                  (merge-with vector $ segment-counts)
                  (vals $)
                  (into {} $))]
    (map
     (comp set (partial map mapping))
     numbers)))

(comment
  ;; part 1
  (reduce + 
          (map
           #(count (keep (set (map segments [1 4 7 8]))
                         (to-canonical %)))
           (input "8.txt")))
  ;; part 2
  (reduce + (map
             #(clojure.edn/read-string
               (apply str (drop-while zero? (map
                                             encode
                                             (to-canonical %)))))
             (input "8.txt"))))
