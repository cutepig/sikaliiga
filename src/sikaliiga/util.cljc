(ns sikaliiga.util
  )

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

(defn period-start? [seconds]
  (some? (#{0 1200 2400 3600} seconds)))
