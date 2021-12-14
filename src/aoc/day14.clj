(ns aoc.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse [f]
  (let [[template _ & rules] (->> (io/resource f)
                                  slurp
                                  str/split-lines)]
    [template
     (into {}
           (map
            (fn [r]
              (let [[i o] (str/split r #" -> ")]
                [(vec i) (first o)]))
            rules))]))

(def sums (partial reduce (fn [acc [k v]]
                            (update acc k (fnil + 0) v)) {}))

(defn resolve-counts
  [template cts]
  (sums
   (cons [(last template) 1]
         (map #(update % 0 first) cts))))

(defn less-naive [[template rules]]
  (let [implications (into {}
                           (for [[[a c :as left] b] rules]
                             [left [[a b] [b c]]]))]
    (letfn [(step [pairs]
                  (sums
                   (mapcat
                    (fn [[chunk ct]]
                      (map #(do [% ct]) (implications chunk)))
                    pairs)))]
      (iterate
       step
       (frequencies (partition 2 1 template))))))

(defn calculate [result]
  (->> result
       vals
       (sort-by -)
       ((juxt first last))
       (apply -)))

(comment
  (let [[template :as input] (parse "14")]
    (calculate
     (resolve-counts
      template
      (nth (less-naive input) 40)))))
