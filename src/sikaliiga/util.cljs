(ns sikaliiga.util
  )

(defn rnd [min max] (+ (rand (- max min)) min))
;; Random int from min (inclusive) to max (exclusive!)
;; @example Get random int in 0-99 (inclusive)
;;   `(irnd 0 100)`
(defn irnd [min max] (int (rnd min max)))

(defn key-by [kw coll]
  (into {} (map #(vector (first %1) (first (second %1))) (group-by kw coll))))

(defn map-values [f coll]
  (into {} (map #(vector (first %) (f (second %))) coll)))
