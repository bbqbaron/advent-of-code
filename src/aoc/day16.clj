(ns aoc.day16
  (:require [clojure.java.io :as io]))

(def hex->bits
  "I was surprised to see a fair amount of online talk recommending just doing this"
  {\0 '(0 0 0 0)
   \1 '(0 0 0 1)
   \2 '(0 0 1 0)
   \3 '(0 0 1 1)
   \4 '(0 1 0 0)
   \5 '(0 1 0 1)
   \6 '(0 1 1 0)
   \7 '(0 1 1 1)
   \8 '(1 0 0 0)
   \9 '(1 0 0 1)
   \A '(1 0 1 0)
   \B '(1 0 1 1)
   \C '(1 1 0 0)
   \D '(1 1 0 1)
   \E '(1 1 1 0)
   \F '(1 1 1 1)})

(defn decode-packet [hexes]
  (mapcat hex->bits hexes))

(defn bits->dec [bits]
  (long (reduce + (map-indexed #(* %2 (Math/pow 2 %1))
                               (reverse bits)))))

(defn packet->version [pkt]
  (bits->dec (take 3 pkt)))

(defn packet->type [pkt]
  (bits->dec (take 3 (drop 3 pkt))))

(declare read-all)

(def ops [:sum :prod :min :max :literal :gt :lt :eq])

(defn read-header [bits]
  (let [version (packet->version bits)
        pkt-type (packet->type bits)]
    [(drop 6 bits)
     [(ops pkt-type) version]]))

(defn read-packet [bits]
  (let [[data [pkt-type :as header]] (read-header bits)
        inner (case pkt-type
                :literal (let [[rem lit-val] (loop [[b & more :as p] data
                                                    v ()]
                                               (case b
                                                 1 (recur (drop 5 p) (concat v (take 4 more)))
                                                 0 [(drop 5 p)
                                                    (concat v (take 4 more))]))]
                           [rem [(bits->dec lit-val)]])
                (let [[[kind] expr-bits] (split-at 1 data)]
                  (case kind
                    0 (let [[sub-pkt-len tail] (update (vec (split-at 15 expr-bits)) 0 bits->dec)
                            [sub-pkt-bits unused] (split-at sub-pkt-len tail)]
                        [unused
                         (read-all sub-pkt-bits)])
                    1 (let [[sub-pkt-ct tail] (update (vec (split-at 11 expr-bits)) 0 bits->dec)
                            [rem sub-pkts] (nth
                                            (iterate
                                             (fn [[rem sub-pkts]]
                                               (update (read-packet rem) 1
                                                       (partial conj sub-pkts)))
                                             [tail []])
                                            sub-pkt-ct)]
                        [rem sub-pkts]))))]
    (update inner 1 (partial conj header))))

(defn read-all [pkt]
  (loop [bits pkt
         out []]
    (if ((every-pred seq (partial not-every? zero?)) bits)
      (let [[rem result] (read-packet bits)]
        (recur rem (conj out result)))
      out)))

(defn version-sum
  [x]
  (cond
    (sequential? x)
    (if (keyword? (first x))
      (+ (second x) (version-sum (last x)))
      (reduce + 0 (map version-sum x)))
    :else 0))

(defn eval-pkt [[k _ sub-exprs]]
  (case k
    :literal (first sub-exprs)
    :sum (reduce + (map eval-pkt sub-exprs))
    :prod (reduce * (map eval-pkt sub-exprs))
    :min (reduce min (map eval-pkt sub-exprs))
    :max (reduce max (map eval-pkt sub-exprs))
    :gt (if (apply > (map eval-pkt sub-exprs)) 1 0)
    :lt (if (apply < (map eval-pkt sub-exprs)) 1 0)
    :eq (if (apply = (map eval-pkt sub-exprs)) 1 0)))

(defn eval-result [pkts]
  (map eval-pkt pkts))

(comment
  (eval-result
   (read-all
    (decode-packet "D2FE28")))

  (eval-result
   (read-all
    (decode-packet "C200B40A82")))

  (read-all
   (decode-packet "38006F45291200"))

  (eval-result (read-all (decode-packet
                          "9C0141080250320F1802104A08")))

  (read-all (decode-packet "EE00D40C823060"))

  (read-all (decode-packet "8A004A801A8002F478"))

  (read-all (decode-packet "620080001611562C8802118E34"))

  (read-all '(0 1 0 1 0 0 0 1 0 1 1 1 0 1 1 0 0 0 1 1 1 1))

  (read-all (decode-packet "C0015000016115A2E0802F182340"))

  (version-sum (read-all (decode-packet "A0016C880162017C3686B18A3D4780")))

  (eval-result
   (read-all
    (decode-packet (-> "16" io/resource slurp)))))
