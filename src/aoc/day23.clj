(ns aoc.day23
  (:require [clojure.set :as set]
            [aoc.prelude :refer [pt mp cp rd fl fs rm ap]]
            [clojure.core.match :refer [match]]
            [malli.core :as mc]
            [malli.dev :as md]
            [clojure.string :as str]))

(def ^:dynamic room-size 2)
(def Species? [:enum :A :B :C :D])
(def State?
  [:map
   [:hall [:vector [:or :nil Species?]]]
   [:rooms [:map-of Species? [:vector [:or :nil Species?]]]]])
(def Loc?
  [:or
   [:tuple [:= :rooms] Species? [:int {:min 0 :max 3}]]
   [:tuple [:= :hall] [:int {:min 0 :max 10}]]])

(defn state->locs [{:keys [hall rooms]}]
  (set
   (concat
    (map
     (partial vector :hall)
     (range (count hall)))
    (mapcat
     (fn [[t]]
       (mapv (partial vector :rooms t) (range room-size)))
     rooms))))
(mc/=> state->locs [:=> [:cat State?] [:set Loc?]])

(def room->hall-idx
  {:A 2 :B 4 :C 6 :D 8})

(defn find-path [state loc dest]
  (match loc
    [:rooms t2 n2]
    (set/union
     (set
      (find-path state [:hall (room->hall-idx t2)] dest))
     (set (mp
           (pt vector :rooms t2)
           (range 0 (inc n2)))))
    [:hall i]
    (match dest
      [:rooms t1 n]
      (set/union
       (set (mp (pt vector :hall)
                (ap range (sort [i (inc (room->hall-idx t1))]))))
       (set (mp
             (partial vector :rooms t1)
             (range 0 (inc n)))))
      [:hall i2]
      (set (mp (pt vector :hall)
               (ap range (sort [i (inc i2)])))))))
(mc/=> find-path [:=> [:cat State? Loc? Loc?] [:set Loc?]])

(defn in-path [state path]
  (set (filter (cp some? (pt get-in state)) path)))
(mc/=> in-path [:=> [:cat State? [:set Loc?]] [:set Loc?]])

(defn home? [state loc]
  (match loc
    [:rooms t n]
    (let [mytype (get-in state loc)]
      (and (every? #{mytype}
                   (fl some? (-> state :rooms t)))
           (= t mytype)
           (every?
            #(some? (get-in state [:rooms t %]))
            (range n room-size))))
    :else false))

(defn move-ok? [state loc dest]
  (match dest
    [:rooms t _]
    (and
     (= t (get-in state loc))
     (every?
      #{(get-in state loc)}
      (fl some? (-> state :rooms t))))
    [:hall di]
    (match loc
      [:hall _] false
      [:rooms & _]
      (not (contains?
            (set (map room->hall-idx (keys (:rooms state))))
            di)))))
(mc/=> move-ok? [:=> [:cat State? Loc? Loc?] :boolean])

(defn blocked? [state from to]
  (->>
   (find-path state from to)
   (in-path state)
   (remove (pt = from))
   seq))

(defn move-opts [state loc]
  (set
   (if
    (home? state loc)
     #{}
     (set/union
      (or
       (let [me (get-in state loc)]
         (when (every?
                #{me}
                (fl some? (-> state :rooms me)))
           (set (take 1
                      (rm
                       (pt blocked? state loc)
                       (mp (pt vector :rooms me)
                           (reverse (range 0 room-size))))))))
       #{})
      (set
       (match loc [:hall _] #{}
              [:rooms _ _]
              (rm (pt blocked? state loc)
                  (mp (pt vector :hall) [0 1 3 5 7 9 10]))))))))
(mc/=> move-opts [:=> [:cat State? Loc?] [:set Loc?]])

(defn get-moves [state]
  (set
   (for [[loc amp] (map (juxt identity (pt get-in state)) (state->locs state))
         :when amp
         o (move-opts state loc)]
     [loc o])))
(mc/=> get-moves [:=> [:cat State?] [:set [:tuple Loc? Loc?]]])

(defn room->hall-cost [mv]
  (match mv
    [:hall _] 0
    [:rooms _ n] (inc n)))

(defn hall-cost [from to]
  (let [[i i2] (map
                #(match %
                   [:hall i] i [:rooms t _] (room->hall-idx t))
                [from to])]
    (Math/abs (- i i2))))

(def step-cost
  {:A 1 :B 10 :C 100 :D 1000})

(defn move-cost [state from to]
  (let [atype (get-in state from)]
    (*
     (step-cost atype)
     (+ (rd + (map room->hall-cost [from to]))
        (hall-cost from to)))))

(defn do-move [state loc dest]
  (->
   state
   (update-in loc (constantly nil))
   (assoc-in dest (get-in state loc))))

(defn transition-options [cost-to current]
  (->> current
       get-moves
       (keep
        (fn [[from to]]
          (let [speculative (+ (cost-to current)
                               (move-cost current from to))
                next-state (do-move current from to)
                current-base (cost-to next-state)]
            (when (or (nil? current-base)
                      (< speculative current-base))
              [next-state speculative]))))
       (into {})))

(defn astar
  "Slightly better than the last time I copied a*"
  [start end]
  (loop [open #{start}
         from {}
         cost-to {start 0}]
    (let [current (->> open (sort-by cost-to) first)]
      (if (= current end)
        (cost-to current)
        (let [goto (transition-options cost-to current)
              open2 (reduce conj (disj open current) (keys goto))]
          (if (seq open2)
            (recur
             open2
             (into from (map #(vector % current) (keys goto)))
             (merge cost-to goto))
            [::failed current]))))))

(defn done-state []
  {:rooms (into {}
                (for [t [:A :B :C :D]]
                  [t (vec (repeat room-size t))]))
   :hall (vec (repeat 11 nil))})

(defn render [state]
  (str/join "\n"
            (into [(ap str (repeat 13 \#))
                   (str \# (ap str (mapv (cp name #(or % ".")) (:hall state))) \#)]
                  (ap mapv str
                      (concat
                       (repeat 3 (ap str (repeat 5 "#")))
                       (into
                        (interleave
                         (for [t [:A :B :C :D]]
                           (str (ap str (mapv name (mp #(or % ".")
                                                       (t (:rooms state))))) "#"))
                         (repeat (ap str (repeat 5 "#")))))
                       (repeat 2 (ap str (repeat 5 "#"))))))))

(comment
  (md/start!
   {:report (fn [_ {:keys [input args output value]}]
              (prn "schema violation"
                   (if input
                     (malli.core/explain input args)
                     (malli.core/explain output value))))})
  

  )
