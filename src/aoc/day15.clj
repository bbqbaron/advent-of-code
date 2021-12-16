(ns aoc.day15
  "Holy heck, part 2 must have run for 2 minutes straight."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse [f]
  (mapv
   (comp (partial mapv edn/read-string) #(str/split % #""))
   (str/split-lines
    (slurp (io/resource f)))))

(defn get-neighbors [size [py px]]
  (for [x (range 1 -1 -1) y (range 1 -1 -1)
        :when (and (some #(not= % 0) [x y])
                   (some zero? [x y]))
        :let [x2 (+ px x) y2 (+ py y)]
        :when (and
               (<= 0 x2 (dec size))
               (<= 0 y2 (dec size)))]
    [y2 x2]))

(defn tally-path [sq from current]
  (->>
   ((fn go [p]
      (if-let [p2 (from p)]
        (cons p (go p2))
        (list p)))
    current)
   reverse
   rest
   frequencies
   (map (fn [[p c]] (* c (get-in sq p))))
   (reduce + 0)))

(defn astar
  "Shameless direct functionalization of imperative a*" 
  [[row :as sq] start end]
  (let [dim (count row)
        current-path-cost (fn [s p] (or (s p) ##Inf))]
    (loop [open #{start}
           from {}
           cost-to {start 0}]
      (if (seq open)
        (let [current (->> open (sort-by cost-to) first)]
          (if (= current end)
            (tally-path sq from current)
            (let [goto (->> current (get-neighbors dim)
                            (keep
                             (fn [neighbor]
                               (let [path-cost (+ (current-path-cost cost-to current)
                                                  (get-in sq neighbor))]
                                 (when (< path-cost (current-path-cost cost-to neighbor))
                                   [neighbor path-cost]))))
                            (into {}))]              
              (recur
               (reduce conj (disj open current) (keys goto))
               (into from (map #(vector % current) (keys goto)))
               (merge cost-to goto)))))
        ::failed))))

(defn lolol-quintuplize [sq]
  (vec
   (mapcat
    (fn [idx-y rows]
      (map
       (fn [row]
         (vec 
          (mapcat
           (fn [idx-x row]
             (map
              (comp
               (fn [chitons]
                 (cond-> chitons
                   (> chitons 9)
                   (-> (mod 10) inc)))
               (partial + idx-x idx-y))
              row))
           (range 5)
           (repeat row))))
       rows))
    (range 5)
    (repeat sq))))

(comment
  (astar
   (lolol-quintuplize (parse "15s"))
   [0 0] [49 49])
  (astar
   (lolol-quintuplize (parse "15"))
   [0 0] [499 499])
  (astar
   (parse "15s")
   [0 0] [9 9])
  (astar
   (parse "15")
   [0 0] [99 99])
  )
