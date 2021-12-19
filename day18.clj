(ns aoc.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [taoensso.tufte :as tf]))

(def pt partial)

(defn prn-it [pref s]
  #_(prn pref (encode s)))

(defn cmp-keys [k k2]
  (or
   (first (drop-while zero? (map compare k k2)))
   0))

(defn pad-k [n d] (vec (take n (concat d (repeat 0)))))

(defn dmax [s]
  (reduce max
          (map
           count
           (keys s))))

(defn read-state [p]
  (let [d ((fn go [d p]
             (if (sequential? p)
               (reduce
                concat
                (map-indexed
                 (fn [i x] (go (conj d (inc i)) x))
                 p))
               [[d p]]))
           [] p)]
    (into {} d)))

(defn pair [s k]
  (let [ks (map
            (pt conj (vec (butlast k)))
            [1 2])
        p (filter second (map (juxt identity s) ks))]
    (when (= 2 (count p))
      p)))

(defn deep-keys [s]
  (->> s keys
       (filter
        #(>= (count %) 5))))

(defn has-pair? [s k]
  (let [p (pair s k)]
    (when (= (count p) 2)
      [k p])))

(defn deeps [s]
  (tf/p ::deeps
        (->> s
             deep-keys
             (keep (pt has-pair? s))
             (sort-by first cmp-keys))))

(defn left [s k]
  (tf/p ::left
        (->> s
             keys
             (filter (comp pos? (pt cmp-keys k)))
             (sort cmp-keys)
             last)))

(defn right [s k]
  (tf/p ::right
        (->> s
             keys
             (filter (comp neg? (pt cmp-keys k)))
             (sort cmp-keys)
             first)))

(defn splode [s]
  (tf/p ::splode
        (when-let [ds (seq (deeps s))]
          (loop [[[dk [[lk] [rk]]] & des] ds
                 seen #{}
                 s s]
            (cond
              (not dk)
              s
              (seen dk)
              (recur des seen s)
              :else
              (recur
               des
               (conj seen dk lk rk)
               (let [l (s lk) r (s rk)
                     ll (left s lk)
                     rr (right s rk)]
                 (cond->
                  (-> s
                      (dissoc lk rk)
                      (assoc (vec (butlast dk)) 0))
                   ll (update ll + l)
                   rr (update rr + r)))))))))

(defn zplit [s]
  (tf/p ::zplit
        (when-let [[k v] (->> s
                              (filter (comp (pt <= 10) second))
                              (sort-by
                               first
                               cmp-keys)
                              first)]
          (-> s
              (dissoc k)
              (assoc (conj (vec k) 1) (int (Math/floor (/ v 2))))
              (assoc (conj (vec k) 2) (int (Math/ceil (/ v 2))))))))

(defn check [s]
  (tf/p ::check
        (or
         (splode s)
         (zplit s)
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
          [(into [(inc i)] k) v])))

(defn encode [s]
  (letfn [(asv [ve [k & ks] v]
            (if (seq ks)
              (update (or ve [nil nil]) (dec k) asv ks v)
              (assoc (or ve [nil nil]) (dec k) v)))]
    (reduce #(asv %1 (remove zero? (first %2)) (second %2)) nil s)))

(defn add [s1 s2]
  (prn-it "a      " s1)
  (prn-it "b      " s2)
  (doto (run (doto (merge-states s1 s2)
               (-> ((pt prn-it "m      ")))))
    (-> ((pt prn-it "c      ")))))

(defn magnitude [s]
  (reduce +
          (for [[k v] s]
            (reduce * v (map #(case % 1 3 2 2 1)
                             (pad-k (dmax s) k))))))

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