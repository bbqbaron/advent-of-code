(ns aoc.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn kind [r]
  (condp re-find r
    #"start" :start
    #"end" :end
    #"[a-z]+" :small
    #"[A-Z]+" :large))

(defn parse [s]
  (assoc 
   (->> s
        str/split-lines
        (mapcat
         (fn [l] (let [[l r] (-> l (str/split #"\-"))]
                   [[l #{r}]
                    [r #{l}]])))
        (reduce (fn [acc [k v]]
                  (update acc k (fnil set/union #{}) v))
                {}))
   "end" #{}))

(defn small-once [state [step :as steps]]
  (let [seen? (frequencies steps)]
    (->> (disj (state step) "start")
         (remove (every-pred
                  (comp #{:small} kind)
                  (comp (every-pred some?
                                    pos?)
                        seen?))))))

(defn small-twice [state [step :as steps]]
  (let [seen? (frequencies steps)
        didit (some (fn [[k v]] (and (#{:small} (kind k))
                                     (> v 1)))
                    seen?)]
    (->> (disj (state step) "start")
         (remove (every-pred
                  (comp #{:small} kind)
                  (comp (every-pred some?
                                    (partial <= (if didit 1 2)))
                        seen?))))))

(defn paths
  [state get-dests]
  (filter 
   (comp #{"end"} first)
   (set
    (letfn [(go [steps]
                (let [ok-dests (get-dests state steps)]
                  (if (seq ok-dests)
                    (mapcat go
                            (map #(cons % steps)
                                 ok-dests))
                    [steps])))]
      (go '("start"))))))

(defn s1 [] (parse (slurp (io/resource "12s"))))
(defn s2 [] (parse (slurp (io/resource "12s2"))))
(defn s3 [] (parse (slurp (io/resource "12s3"))))
(defn input [] (parse (slurp (io/resource "12"))))

(comment
  (assert (= 10 (count (paths (s1) small-once))))
  (assert (= 19 (count (paths (s2) small-once))))
  (assert (= 226 (count (paths (s3) small-once))))
  (count (paths (input) small-once))

  (assert (= 36 (count (paths (s1) small-twice))))
  (assert (= 103 (count (paths (s2) small-twice))))
  (assert (= 3509 (count (paths (s3) small-twice))))
  (count (paths (input) small-twice)))
