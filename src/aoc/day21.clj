(ns aoc.day21
  (:require [aoc.prelude :refer [pt cp rd mp ap freq-by group mv sum fl fs sd ud ud-in]]))

(def rollover
  (cp #(case % 0 10 %) #(mod % 10)))

;; part 1, totally different model

(defn scores [starting player-number]
  (reductions
   +
   (map
    rollover
    (rest
     (reductions + (cons starting (drop (+ 1 player-number)
                                        (cycle (range (+ 8 player-number) -1 -2)))))))))

(defn state [p1 p2]
  (map vector
       (map inc (range))
       (partition 2 1 (interleave (scores p1 0)
                                  (scores p2 1)))))

(defn game [p1 p2]
  (let [[tn [loser]]
        (first (drop-while
                (cp (pt > 1000) second second)
                (state p1 p2)))]
    (* 3 (inc tn) loser)))

;; part 2, lol start over

(def roll-distro (into {}
                       (map
                        #(update % 1 count)
                        (group-by
                         (pt ap +)
                         (let [[xs ys zs] (repeat 3 (range 1 4))]
                           (for [x xs y ys z zs]
                             [x y z]))))))

(defn pos->moves [roll-distro pos]
  (for [[k v] roll-distro
        :let [new-pos (rollover (+ pos k))]]
    [new-pos v]))

(defn score->moves [roll-distro [s positions]]
  (mv
   (fn [xs]
     (rd (fn [acc [k v]]
           (ud acc k (fnil + 0) v))
         {} xs))
   (group
    (for [[pos weight] positions
          [new-pos weight2] (pos->moves roll-distro pos)]
      [(+ s new-pos) new-pos (* weight weight2)]))))

(defn speculate [roll-distro player-states]
  (mv
   (fn [xs]
     (rd (fn [acc [k v]]
           (ud acc k (fnil + 0) v))
         {}
         (reduce concat
                 (mapcat vec xs))))
   (group (mapcat (pt score->moves roll-distro) player-states))))

(defn step1 [roll-distro winscore [tn ws ps]]
  (let [id (mod tn 2)
        p (ps id)
        uc1 (rd + (map (cp (pt rd +) vals) (vals p)))
        s (reduce
           (fn [acc [score universes]]
             (let [ucount (rd + (mp sd universes))]
               (cond (>= score winscore)
                     (-> acc
                         (update-in [1 id] + ucount))
                     :else
                     (-> acc
                         (assoc-in [2 id score] universes)
                         (ud 3 + ucount)))))
           [(inc tn) ws (assoc ps id {}) 0]
           (speculate roll-distro p))]
    (vec (butlast (ud-in s [2 (mod (inc id) 2)]
                         (pt mv (pt mv (pt *
                                           (if (pos? uc1)
                                             (/
                                              (last s)
                                              uc1)
                                             0)))))))))

(defn run-game [roll-distro wn p1 p2]
  (try
    (let [step (pt step1 roll-distro wn)
          [moves done] (split-with
                        (cp (pt some seq) #(nth % 2))
                        (take
                         100
                         (iterate
                          step
                          [0 [0 0]
                           [{0 {p1 1}}
                            {0 {p2 1}}]])))]
      (concat moves (take 1 done)))
    (catch Throwable t
      (prn t))))

(comment
  (run-game roll-distro 21 4 8))
