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
  (for [x (range -1 2) y (range -1 2)
        :when (and (some #(not= % 0) [x y])
                   (some zero? [x y]))
        :let [x2 (+ px x) y2 (+ py y)]
        :when (and
               (<= 0 x2 (dec size))
               (<= 0 y2 (dec size)))]
    [y2 x2]))

(defn cost-of [sq from current]
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
        cost (partial get-in sq)
        get-g (fn [s p] (or (s p) ##Inf))]
    (loop [open #{start}
           from {}
           gscore {start 0}
           fscore {start (cost start)}]      
      (if (seq open)
        (let [current (->> open (sort-by fscore) first)]
          (if (= current end)
            (cost-of sq from current)
            (let [{:keys [open from gscore fscore]}
                  (reduce
                   (fn reduce-state [{:keys [gscore] :as acc} neighbor]
                     (let [tentative (reduce + 0
                                             (filter some?
                                                     [(get-g gscore current) (cost neighbor)]))]
                       (if (some->> (get-g gscore neighbor) (< tentative))
                         (-> acc
                             (update :from assoc neighbor current)
                             (update :gscore assoc neighbor tentative)
                             (update :fscore assoc neighbor
                                     (reduce + 0 (filter some? [tentative (cost neighbor)])))
                             (update :open conj neighbor))
                         acc)))
                   {:from from
                    :gscore gscore
                    :fscore fscore
                    :open (disj open current)}
                   (get-neighbors dim current))]
              (recur
               open from gscore fscore))))
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
