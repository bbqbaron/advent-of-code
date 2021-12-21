(ns aoc.day19
  (:require
   [aoc.prelude :refer :all]
   [clojure.set :as set]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.edn :as edn]))

(defn feasible-offset [group-size axis l r]
  (filter some?
          (let [ss (set (map #(% axis) l))]
            (for [fixed-l ss fixed-r (map #(nth % axis) r)
                  :let [offset (- fixed-l fixed-r)
                        got (>=
                             (count
                              (filter some?
                                      (for [x r]
                                        (ss (+ (x axis) offset)))))
                             group-size)]]
              (when got offset)))))

(defn apply-hypothesis [by pt]
  (mapv
   (fn [k]
     (if-let [[a _ f?] (by k)]
       (let [v2 (pt a)]
         (cond-> v2 f? -))
       (pt k)))
   (range 3)))

(defn match-step [group-size ms m2s exclude-l exclude-r hypothesis]
  (let [as (remove exclude-l (range 3))
        as2 (remove exclude-r (range 3))]
    (if (every? seq [as as2])
      (keep identity
            (for [a as
                  a2 as2
                  flip? [false true]
                  :let [hypothesis (assoc hypothesis a [a2 nil flip?])
                        hypothesized
                        (set (map (pt apply-hypothesis hypothesis) m2s))]
                  o (feasible-offset group-size
                                     a
                                     ms
                                     hypothesized)
                  :let [h2 (-> hypothesis
                               (assoc-in
                                [a 1] o))]
                  res (match-step group-size ms m2s
                                  (conj exclude-l a)
                                  (conj exclude-r a2)
                                  h2)]
              (into h2 res)))
      [nil])))

(defn solve-coords [group-size ms m2s]
  (first (match-step group-size ms m2s #{} #{}
                     {0 [0 0 false]
                      1 [1 0 false]
                      2 [2 0 false]})))

(defn parse [f]
  (mapv
   (cp
    (fn [l] (let [s (set l)]
              (when (not= (count s) (count l))
                (throw (ex-info "shit" {})))
              s))
    (pt mapv (pt mapv edn/read-string))
    (pt mapv #(str/split % #","))
    (pt remove str/blank?) str/split-lines)
   (remove
    str/blank?
    (str/split
     (slurp (io/resource f))
     #"--- scanner \d+ ---"))))

(defn left->right [by pt]
  (vec
   (map second
        (sort-by first
                 (map-indexed
                  (fn [axis v]
                    (if-let [[other-axis [_ offset flip?]]
                             (->> by
                                  (filter (cp #{axis} first second))
                                  first)]
                      (let [v (pt other-axis)]
                        [axis (cond-> (- v offset) flip? -)])
                      [axis v]))
                  pt)))))

(defn right->left [by pt]
  (mapv
   (fn [axis]
     (if-let [[other-axis offset flip?] (by axis)]
       (let [left (pt other-axis)]
         (+ (cond-> left flip? -) offset))
       (pt axis)))
   (range 3)))

(defn get-to [what schemes pts n]
  ((fn go [seen pts n]
     (first
      (filter
       some?
       (for [[[l r :as pair] {:keys [relation]}]
             (->> schemes (filter (cp (every-pred
                                       (pt (complement seen))
                                       (pt some #{n})) first)))]
         (cond
           (= l what)
           (set (map (pt right->left relation) pts))
           (= r what)
           (set (map (pt left->right relation) pts))
           (= l n)
           (go (conj seen pair)
               (set (map (pt left->right relation) pts))
               r)
           (= r n)
           (go (conj seen pair)
               (set (map (pt right->left relation) pts))
               l))))))
   #{} pts n))

(def ->zero (partial get-to 0))

(defn gather-offsets [input]
  (for [k (range (count input))
        k2 (remove (pt >= k) (range (count input)))]
    (let [rel (solve-coords 12 (nth input k) (nth input k2))]
      (when rel
        {:left k
         :right k2
         :relation rel}))))

(defn mk-offset-index [inp]
  (into {}
        (map #(update % 1 first)
             (group-by (juxt :left :right) (filter some? (gather-offsets inp))))))

(defn count-beacons [[h :as input]]
  (let [scheme-index (mk-offset-index input)]
    (count
     (apply set/union
            h
            (for [n (range 1 (count input))]
              (->zero scheme-index (nth input n) n))))))

(comment
  (def sample-input (parse "19s"))
  (def real-input (parse "19"))
  (count-beacons real-input)
  (count-beacons sample-input)
  (defn max-distance [input]
    (last 
     (sort
      (let [si (mk-offset-index input)]
        (for [i (range (count input))
              i2 (remove (pt >= i) (range (count input)))]
          (->> (get-to i2 si #{[0 0 0]} i)
               first
               (map #(Math/abs %))
               (reduce +)))))))
  (max-distance sample-input)
  )
