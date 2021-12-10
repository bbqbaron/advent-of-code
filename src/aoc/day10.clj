(ns aoc.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- input [file]
  (->> file
       io/resource
       slurp
       str/split-lines))

(def by-open {\( \) \[ \] \{ \} \< \>})
(def score {\) 3 \] 57 \} 1197 \> 25137})
(def closing-score {\) 1 \] 2 \} 3 \> 4})

(defn consume [line]
  (letfn [(go [[s & ss :as stack] [l & ls]]
              (if l
                (if-let [close (by-open l)]
                  (recur (cons close stack) ls)
                  (if (not= l s)
                    l
                    (recur ss ls)))
                stack))]
    (go () line)))

(defn score-line [l]
  (reduce (fn [acc c] (+ (closing-score c) (* 5 acc))) 0 l))

(defn winner [s] (nth s (/ (count s) 2)))

(comment
  (reduce + (map score (filter char? (map consume (input "10s.txt")))))
  (reduce + (map score (filter char? (map consume (input "10.txt")))))
  (winner (sort (map score-line (filter sequential? (map consume (input "10.txt"))))))
  (winner (sort (map score-line (filter sequential? (map consume (input "10s.txt"))))))
  (consume "(]"))
