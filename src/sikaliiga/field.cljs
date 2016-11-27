(ns sikaliiga.field
  (:require [clojure.spec :as s]
            [sikaliiga.player :as player]))

(s/def ::index integer?)
(s/def ::shift-forwards integer?)
(s/def ::shift-defenders integer?)
(s/def ::goalie (s/nilable uuid?))
(s/def ::defenders (s/nilable (s/tuple uuid? uuid?)))
(s/def ::forwards (s/coll-of uuid? :kind list? :min-count 2 :max-count 4))
(s/def ::players (s/tuple ::goalie ::defenders ::forwards))
(s/def ::field (s/keys :req-un [::index ::shift-forwards ::shift-defenders ::players]))

(defn get-goalie [field]
  (first (:players field)))

(defn get-defenders [field]
  (second (:players field)))

(defn get-defender [field n]
  (nth (get-defenders field) n))

(defn get-forwards [field]
  (nth (:players field) 2))

(defn get-left-wing [field]
  (first (get-forwards field)))

(defn get-center [field]
  (second (get-forwards field)))

(defn get-right-wing [field]
  (nth (get-forwards field) 2))

(defn get-extra-forward [field]
  (nth (get-forwards field) 3))

;; `field-out` prepared field consisting of actual players instead of id's.
;; Map `field-out` stats from `team` back to `new-team`.
;; Prepare `field-in` consisting of actual players instead of id's
;; Return `[new-team field-in]`
(defn shift-field-forwards [secs team field-out]
  ;; TODO: Powerplay
  ;; TODO: Shorthand
  ;; TODO: Skip injured players or players on penalty
  (let [next-index (mod (inc (or (:index-forwards field-out) -1)) 4)
        next-field (get-in team [:fields next-index])
        forwards (map #(player/by-id team %) (get-forwards next-field))
        field-in (-> field-out
                     (assoc-in [:players 2] forwards)
                     (assoc :shift-forwards (:shift-forwards next-field))
                     ;; TODO: Slight variance to this
                     (assoc :next-shift-forwards (+ secs (:shift-forwards next-field)))
                     (assoc :index-forwards next-index))]
    [team field-in]))

;; Initial field can be set by giving `nil` as `field-out`
(defn shift-field-defenders [secs team field-out]
  ;; TODO: Powerplay
  ;; TODO: Shorthand
  ;; TODO: Skip injured players or players on penalty
  (let [next-index (mod (inc (or (:index-defenders field-out) -1)) 3)
        next-field (get-in team [:fields next-index])
        defenders (map #(player/by-id team %) (get-defenders next-field))
        field-in (-> field-out
                     (assoc-in [:players 1] defenders)
                     (assoc :shift-defenders (:shift-defenders next-field))
                     ;; TODO: Slight variance to this
                     (assoc :next-shift-defenders (+ secs (:shift-defenders next-field)))
                     (assoc :index-defenders next-index))]
    [team field-in]))

(defn pull-field-goalie [secs team field-out]
  ;; Null goalie and return
  [team (assoc-in field-out [:players 0] nil)])

(defn shift-forwards? [secs field]
  (>= secs (:next-shift-forwards field)))

(defn shift-defenders? [secs field]
  (>= secs (:next-shift-defenders field)))

(defn auto-field-nth [goalies defenders forwards n]
  [(:id (first goalies))
   (map :id (take 2 (drop (* 2 n) defenders)))
   (map :id (take 3 (drop (* 3 n) defenders)))])

(defn auto-fields [team]
  (let [players (vals (:players team))
        goalies (->> players (filter player/goalie?) (sort-by :defense) reverse)
        defenders (->> players (filter player/defender?) (sort-by :defense) reverse)
        forwards (->> players (filter player/forward?) (sort-by :attack) reverse)]
    ;; FIXME: Handle left-, right-wings and center correctly
    [{:index 0 :shift-forwards 40 :shift-defenders 50 :players (auto-field-nth goalies defenders forwards 0)}
     {:index 1 :shift-forwards 30 :shift-defenders 40 :players (auto-field-nth goalies defenders forwards 1)}
     {:index 2 :shift-forwards 20 :shift-defenders 30 :players (auto-field-nth goalies defenders forwards 2)}
     {:index 3 :shift-forwards 20 :players (auto-field-nth goalies defenders forwards 3)}]))
