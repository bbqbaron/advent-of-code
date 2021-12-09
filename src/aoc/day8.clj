(ns aoc.day8
  "I hate this entire solution. I feel like an idiot. 
   Watch this be a two-liner when done correctly."
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
                       (partial map (partial map (comp keyword str)))
                       (partial remove str/blank?)
                       #(str/split % #"\s+")))
             #(str/split % #"\|")))))

(defn- encode
  [s] (ffirst (filter (comp #{s} second) segments)))

(defn- unique-partitions
  "I have no idea what this algo is supposed to be called."
  [input]
  (letfn [(go [seen [x & xs]] (if (seq xs)
                                (for [o (remove seen x)
                                      sub (go (conj seen o) xs)]
                                  (cons o sub))
                                (map list (remove seen x))))]
    (go #{} input)))

(def possible-digits
  (reduce (partial merge-with set/union)
          (for [[i u] segments]
            {(count u) #{i}})))

(def all-segments (sort (set (reduce concat (vals segments)))))

(defn- fuck-it [states signal]
  (letfn [(process-digit [state segment-options digit-option]
                         (let [for-digit (map
                                          (fnil (partial set/intersection digit-option)
                                                digit-option)
                                          segment-options)]
                           (when (every? not-empty for-digit)
                             (let [segment-facts (->
                                                  (into state (map vector signal for-digit))
                                                  (map all-segments)
                                                  ;; soul leaving my body.
                                                  ;; smash face on keyboard, make computer
                                                  ;; do math thingy.
                                                  unique-partitions)
                                   refined-state (map
                                                  (comp (partial into {}) (partial map vector all-segments)
                                                        (partial map hash-set))
                                                  segment-facts)
                                   new-facts (reduce
                                              (partial merge-with set/union)
                                              refined-state)]
                               new-facts))))
          (run-state [state]
                     (let [digit-options (map segments (possible-digits (count signal)))
                           segment-options (map state signal)]
                       (filter seq
                               (map (partial process-digit state segment-options)
                                    digit-options))))]
    (mapcat run-state states)))

(comment
  (reduce +
          (map
           (comp clojure.edn/read-string
                 ;; lol, whatever
                 #(str/replace % #"^0+" "")
                 (partial apply str))
           (for [[clues answer] (input "8.txt")]
             (let [init (into {}
                              (map
                               vector
                               all-segments
                               (repeat (set all-segments))))                   
                   solved (into {}
                                (map
                                 #(update % 1 first)
                                 (first
                                  (reduce fuck-it [init] clues))))]
               (map
                (comp encode set (partial map solved))
                answer))))))
