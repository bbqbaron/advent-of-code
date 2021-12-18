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

(defn ek [d k]
  (reduce + (map-indexed (fn [i v]
                           (* v (Math/pow 3 i)))
                         (reverse (pad-k d k)))))

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
       (sort cmp-keys)
       (filter
        #(>= (count %) 5))))

(defn has-pair? [s k]
  (let [p (pair s k)]
    (when (= (count p) 2)
      [k p])))

(defn deep? [s]
  (->> s
       deep-keys
       (some (pt has-pair? s))))

(defn left [s k]
  (let [dm (dmax s)
        nk (ek dm k)]
    (->> s
         keys
         (filter (comp (pt > nk) (pt ek dm)))
         (sort cmp-keys)
         last)))

(defn right [s k]
  (let [dm (dmax s)
        nk (ek dm k)]
    (->> s
         keys
         (filter (comp (pt < nk) (pt ek dm)))
         (sort cmp-keys)
         first)))

(defn splode [s]
  (if-let [[dk [[lk l] [rk r]]] (deep? s)]
    (let [ll (left s lk)
          rr (right s rk)]
      (cond->
       (-> s
           (dissoc lk rk)
           (assoc (vec (butlast dk)) 0))
        ll (update ll + l)
        rr (update rr + r)))
    s))

(defn zplit [s]
  (if-let [[k v] (first (filter (comp (pt <= 10) second)
                                (sort-by
                                 first
                                 cmp-keys
                                 s)))]
    (-> s
        (dissoc k)
        (assoc (conj (vec k) 1) (int (Math/floor (/ v 2))))
        (assoc (conj (vec k) 2) (int (Math/ceil (/ v 2)))))
    s))

(defn check [s]
  (let [s2 (splode s)]
    (if (= s s2)
      (let [s3 (zplit s)]
        (if (not= s s3)
          (do
            (prn-it "split  " s)
            s3)
          s))
      (do
        (prn-it "explode" s2)
        s2))))

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
