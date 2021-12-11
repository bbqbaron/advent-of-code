(ns aoc.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn- input [file]
  (->> file
       io/resource
       slurp
       str/split-lines
       (mapv (comp (partial mapv edn/read-string)
                   #(str/split % #"")))))

(defn step [octopuses]
  (letfn [(check-flash [{:keys [flashed octopuses] :as acc} id]
                       (let [[energy neighbors] (octopuses id)]
                         (cond-> acc
                           (and (not (flashed id)) (> energy 9))
                           (-> (update :flashed conj id) (cascade neighbors)))))
          (bump [state id]
                (check-flash (update-in state [:octopuses id 0] inc) id))
          (cascade [state ids]
                   (reduce bump state ids))]
    (let [{:keys [flashed] os3 :octopuses}
          (cascade {:flashed #{} :octopuses octopuses} (keys octopuses))]
      {:flashes (count flashed)
       :octopuses (reduce #(assoc-in %1 [%2 0] 0) os3 flashed)})))

(defn run [octopuses]
  (iterate
   (fn [{:keys [flashes octopuses]}]
     (let [{flashes2 :flashes
            os2 :octopuses} (step octopuses)]
       {:flashes (+ flashes flashes2)
        :octopuses os2}))
   {:flashes 0 :octopuses octopuses}))

(defn first-synchronized [octopuses]
  (inc
   (first
    (keep
     (fn [[i [x y]]] (when (= 100 (- y x)) i))
     (map-indexed vector
                  (partition 2 1
                             (map :flashes (run octopuses))))))))

(defn get-octopuses [file] (into {}
                                 (let [input (input file)]
                                   (for [x (range (count input))
                                         y (range (count input))]
                                     (let [octopus [[y x] [(get-in input [y x])
                                                           (->>
                                                            (for [dx (range -1 2)
                                                                  dy (range -1 2)]
                                                              [(+ y dy) (+ x dx)])
                                                            (filter (partial get-in input))
                                                            (remove #{[y x]}))]]]
                                       octopus)))))

(defn debug [{:keys [octopuses]}]
  (->> (for [[id [energy]] octopuses]
         [id (case energy 0 "Z" energy)])
       (sort-by first)
       (partition 10)
       (map (comp str/join (partial map second)))
       (str/join "\n")
       println))

(comment
  (debug (nth (run (get-octopuses "11s.txt")) 2))
  (:flashes (nth (run (get-octopuses "11s.txt")) 10))
  (:flashes (nth (run (get-octopuses "11s.txt")) 100))
  (:flashes (nth (run (get-octopuses "11.txt")) 100))
  (first-synchronized (get-octopuses "11s.txt"))
  (first-synchronized (get-octopuses "11.txt")))
