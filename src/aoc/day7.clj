(ns aoc.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn input [file]
  (map
   clojure.edn/read-string
   (-> file
       io/resource
       slurp
       str/split-lines)))

(defn binary-search
  [distance->cost ps]
  (let [freqs (frequencies ps)]
    (letfn [(point->cost [point]
              (reduce
               +
               (map
                (fn [[point2 n]]
                  (let [distance (Math/abs (- point point2))]
                    (* n (distance->cost distance))))
                freqs)))
            (step [left right]
              (if (= left right)
                (point->cost left)
                (let [left2 (+ left (int (* (max 0 (- right left)) 0.5)))
                      right2 (inc left2)
                      [left-cost right-cost] (map point->cost [left2 right2])]
                  (if (< left-cost right-cost)
                    (recur left left2)
                    (recur right2 right)))))]
      (step 0 (reduce max ps)))))

(defn summed [x] (* x (inc x) 0.5))

(comment
  (binary-search identity (input "7sample.txt"))
  (binary-search summed (input "7.txt")))
