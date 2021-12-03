(ns aoc.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input [& [lines]]
  (map (partial map #(case % \0 0 \1 1))
       (str/split-lines
        (or lines
            (slurp (io/resource "3.txt"))))))

(defn most-common-bits []
  (for
   [[zs os]
    (reduce
     (fn [acc bitcounts]
       (map
        (partial map +)
        acc
        bitcounts))
     (repeat 12 [0 0])
     (map
      (partial map
               #(case % 0 [1 0] 1 [0 1]))
      (input)))]
    (if (> zs os) 0 1)))

(defn encode [x]
  (reduce +
          (map * (reverse x)
               (iterate (partial * 2) 1))))

(defn part1 [g]
  (let [e (for [n g]
            (case n 0 1 1 0))]
    (apply * (map encode [g e]))))

(comment
  (part1 (most-common-bits)))

;; TBH doing this naively would probably have been fine.
;; the problem just seemed trieish to me, so why not?

(defn add-node [acc [c & cs]]
  (cond-> acc c
          (update c
                  (fn [{:keys [ct children]}]
                    {:ct ((fnil inc 0) ct)
                     :children (add-node children cs)}))))

(defn bit-trie []
  (reduce
   add-node
   {}
   (input)))

(defn extract-only [trie]
  (when trie
    (let [[k {:keys [children]}] (first trie)]
      (into [k] (extract-only children)))))

(defn normal-number [most-or-least tie-winner og-trie]
  (letfn [(most-common-entry [trie]
            (let [sorter (comp (case most-or-least :most - :least identity)
                               first)]
              (->> trie
                   (group-by (comp :ct second))
                   (sort-by sorter)
                   first
                   second)))
          (step [trie bits]
            (let [[winner tie :as results]
                  (most-common-entry trie)
                  [bit {:keys [ct children]}]
                  (if tie
                    (first (filter (comp #{tie-winner} first) results))
                    winner)
                  newnum (conj bits bit)]
              (case ct
                1 (into newnum (extract-only children))
                (recur children newnum))))]
    (step og-trie [])))

(defn part2 []
  (let [bt (bit-trie)]
    (* (encode (normal-number :most 1 bt))
       (encode (normal-number :least 0 bt)))))

(comment
  (part2))