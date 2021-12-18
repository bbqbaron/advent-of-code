(ns aoc.day17)

(defn sign [x] (if (neg? x) -1 1))
(defn summation [x] (* (sign x) (/ (* x ((if (pos? x) inc dec) x)) 2)))

(defn pos [vx vy t]
  [(- (summation vx)
      (summation (max 0 (- vx t))))
   (- (* t vy) (summation t))])

(defn get-all [[xl xr] [yl yr]]
  (set
   (filter some?
           (for [vx (range 1 (inc xr))
                 vy (range yl (inc (- yl)))]
             (when (some (fn [[x2 y2]]
                           (and (<= xl x2 xr)
                                (<= yl y2 yr)))
                         (take-while
                          (fn [[x2 y2]]
                            (and (<= x2 xr)
                                 (<= yl y2)))
                          (map (partial pos vx vy) (range))))
               [vx vy])))))

(prn (count (get-all [20 30] [-10 -5])))
