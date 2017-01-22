(ns sikaliiga.util
  #?(:cljs
     (:import goog.string.format)))

(defn rnd [min max] (+ (rand (- max min)) min))
;; Random int from min (inclusive) to max (exclusive!)
;; @example Get random int in 0-99 (inclusive)
;;   `(irnd 0 100)`
(defn irnd [min max] (int (rnd min max)))

;; Fix rand-nth crashing on empty collection
(defn rnd-nth [coll]
  (if (empty? coll) nil (rand-nth coll)))

(defn key-by [kw coll]
  (into {} (map #(vector (first %1) (first (second %1))) (group-by kw coll))))

(defn map-values [f coll]
  (into {} (map #(vector (first %) (f (second %))) coll)))

#?(:clj
   (defn make-uuid []
     (java.util.UUID/randomUUID))
   :cljs
   (defn make-uuid []
     (random-uuid)))

(defn mod-to-range
  "mod n so that min <= n <= max"
  [n min max]
  (+ min (mod (- n min) (- (inc max) min))))

(defn str-left-pad-num [count num]
  (format (str "%0" count "d") num))
