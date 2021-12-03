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
   [freq
    (map frequencies (apply map vector (input)))
    :let [zs (freq 0) os (freq 1)]]
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
                             (let [options (map (juxt first (comp :ct second)) trie)
                                   bit (if (apply = (map second options))
                                         tie-winner
                                         (first 
                                          ((case most-or-least :most last first)
                                           (sort-by second options))))]
                               [bit (trie bit)]))
          (step [trie bits]
            (let [[bit {:keys [ct children]}]
                  (most-common-entry trie)
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