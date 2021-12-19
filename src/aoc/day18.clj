(ns aoc.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [taoensso.tufte :as tf]))

(def pt partial)

(defn prn-it [pref s]
  #_(prn pref (encode s)))

(defn compare-lists [k k2]
  (or
   (first (drop-while zero? (map compare k k2)))
   0))

(defn pad-key [depth key] (vec (take depth (concat key (repeat nil)))))

(defn depth-max [state]
  (reduce max
          (map
           count
           (keys state))))

(defn read-state [pairs]
  (into {} ((fn go [d p]
              (if (sequential? p)
                (reduce
                 concat
                 (map-indexed
                  (fn [i x] (go (conj d i) x))
                  p))
                [[d p]]))
            [] pairs)))

(defn pair [state key]
  (let [partner-keys (map
                      (pt conj (vec (butlast key)))
                      [0 1])
        found (filter second (map (juxt identity state) partner-keys))]
    (when (= 2 (count found))
      found)))

(defn deep-keys [state]
  (->> state 
       keys
       (filter #(>= (count %) 5))))

(defn has-pair? [state key]
  (let [p (pair state key)]
    (when (= (count p) 2)
      [key p])))

(defn exploding-keys [state]
  (tf/p ::deeps
        (->> state
             deep-keys
             (keep (pt has-pair? state))
             (sort-by first compare-lists))))

(defn left-of [state key]
  (tf/p ::left
        (->> state
             keys
             (filter (comp pos? (pt compare-lists key)))
             (sort compare-lists)
             last)))

(defn right-of [state key]
  (tf/p ::right
        (->> state
             keys
             (filter (comp neg? (pt compare-lists key)))
             (sort compare-lists)
             first)))

(defn explode [state]
  (tf/p ::splode
        (when-let [ks (seq (exploding-keys state))]
          (loop [[[key [[left-key] [right-key]]] & more] ks
                 seen #{}
                 s state]
            (cond
              (not key)
              s
              (seen key)
              (recur more seen s)
              :else
              (recur
               more
               (conj seen key left-key right-key)
               (let [left-val (s left-key) right-val (s right-key)
                     left-left (left-of s left-key)
                     right-right (right-of s right-key)]
                 (cond->
                  (-> s
                      (dissoc left-key right-key)
                      (assoc (vec (butlast key)) 0))
                   left-left (update left-left + left-val)
                   right-right (update right-right + right-val)))))))))

(defn split-large [state]
  (tf/p ::split
        (when-let [[k v] (->> state
                              (filter (comp (pt <= 10) second))
                              (sort-by
                               first
                               compare-lists)
                              first)]
          (-> state
              (dissoc k)
              (assoc (conj (vec k) 0) (int (Math/floor (/ v 2))))
              (assoc (conj (vec k) 1) (int (Math/ceil (/ v 2))))))))

(defn check [s]
  (tf/p ::check
        (or
         (explode s)
         (split-large s)
         s)))

(defn run [s]
  (second
   (first
    (drop-while
     (pt apply not=)
     (partition-all 2 1 (iterate check s))))))

(defn merge-states [s1 s2]
  (into {}
        (for [[i s] (map-indexed vector [s1 s2])
              [k v] s]
          [(into [i] k) v])))

(defn encode 
  "Just debugging"
  [state]
  (letfn [(assoc-vec [target [k & ks] v]
            (if (seq ks)
              (update (or target [nil nil]) k assoc-vec ks v)
              (assoc (or target [nil nil]) k v)))]
    (reduce #(assoc-vec %1 (first %2) (second %2)) nil state)))

(defn add [s1 s2]
  (prn-it "a      " s1)
  (prn-it "b      " s2)
  (doto (run (doto (merge-states s1 s2)
               (-> ((pt prn-it "m      ")))))
    (-> ((pt prn-it "c      ")))))

(defn magnitude [s]
  (reduce +
          (for [[k v] s]
            (reduce * v (map #(case % 0 3 1 2 1)
                             (pad-key (depth-max s) k))))))

(defn run-case [xs]
  ((juxt magnitude encode)
   (reduce add (map read-state xs))))

(def input (map edn/read-string (str/split-lines (slurp (io/resource "18")))))

(tf/add-basic-println-handler! {})
(comment
  (tf/profile {}
              (assert
               (= 3725 (first (run-case input)))))

  (tf/profile {}
              (assert 
               (= 4832 
                  (reduce max
                        (for [x input
                              y (remove (pt = x) input)]
                          (first (run-case [x y]))))))))