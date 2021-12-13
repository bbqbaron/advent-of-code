(ns aoc.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse [f]
  (let [[pts _ folds] (partition-by str/blank?
                                    (->> (io/resource f)
                                         slurp
                                         str/split-lines))]
    [(map
      (comp
       (partial mapv edn/read-string)
       #(str/split % #","))
      pts)
     (map
      (comp
       (fn [[_ axis aval]]
         [axis (edn/read-string aval)])
       (partial re-find #"fold along ([xy])=(\d+)"))
      folds)]))

(defn visible? [[x y]]
  (every? #(>= % 0) [x y]))

(defn fold-em [axis aval pts]
  (->>
   pts
   (map
    #(update % axis
             (fn [v]
               (cond-> v (> v aval)
                       (-> (- aval) - (+ aval))))))
   set
   (filter visible?)))

(defn width [pts] (inc (reduce max (map first pts))))
(defn height [pts] (inc (reduce max (map second pts))))

(let [[pts folds] (parse "13")
      pts (reduce
           (fn [pts [axis aval]]
             (case axis
               "x" (fold-em 0 aval pts)
               "y" (fold-em 1 aval pts)))
           pts
           folds)
      xmax (width pts)
      ymax (height pts)
      grid (mapv vec (repeat ymax (repeat xmax " ")))]
  (println 
   (str/join
    "\n"
    (map
     str/join
     (reduce
      (fn [acc [x y]] (assoc-in acc [y x] "X"))
      grid
      pts)))))
