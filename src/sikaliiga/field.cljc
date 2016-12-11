(ns sikaliiga.field
  (:require [clojure.spec :as s]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]))

(s/def ::index integer?)
(s/def ::shift-forwards integer?)
(s/def ::shift-defenders integer?)
(s/def ::goalie (s/nilable uuid?))
(s/def ::defenders (s/nilable (s/tuple uuid? uuid?)))
(s/def ::forwards (s/coll-of uuid? :kind list? :min-count 2 :max-count 4))
(s/def ::players (s/tuple ::goalie ::defenders ::forwards))
(s/def ::field (s/keys :req-un [::index ::shift-forwards ::shift-defenders ::players]))

(defn get-players [field]
  (flatten (rest (:players field))))

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

(defn get-player-by-index [field index]
  (cond
    (> index 2) (nth (nth field 2) (- index 3))
    (> index 0) (nth (second field) (dec index))
    :else (first field)))

(defn get-position-by-index [index]
  ;; FIXME: That pesky `::player/extra-attacker` is probably not the correct value to put here
  (nth [::player/goalie ::player/defense ::player/defense ::player/left-wing ::player/center ::player/right-wing ::player/extra-attacker] index))

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

(defn power-play-forwards? [team]
  (let [field-index (:current-field-forwards team)]
    (or (= 4 field-index) (= 5 field-index))))

(defn power-play-defenders? [team]
  (let [field-index (:current-field-defenders team)]
    (or (= 3 field-index) (= 4 field-index))))

(defn short-handed-forwards? [team]
  (let [field-index (:current-field-forwards team)]
    (or (= 6 field-index) (= 7 field-index))))

(defn short-handed-defenders? [team]
  (let [field-index (:current-field-defenders team)]
    (or (= 5 field-index) (= 6 field-index))))

(defn shift-forwards? [state team]
  (or (util/period-start? (:seconds state))
      (>= (:seconds state) (or (:next-shift-forwards team) 0))
      (and (:power-play? team) (not (power-play-forwards? team)))
      (and (:short-handed? team) (not (short-handed-forwards? team)))
      (and (not (:power-play? team)) (power-play-forwards? team))
      (and (not (:short-handed? team)) (short-handed-forwards? team))))

(defn shift-defenders? [state team]
  (or (util/period-start? (:seconds state))
      (>= (:seconds state) (or (:next-shift-defenders team) 0))
      (and (:power-play? team) (not (power-play-defenders? team)))
      (and (:short-handed? team) (not (short-handed-defenders? team)))
      (and (not (:power-play? team)) (power-play-defenders? team))
      (and (not (:short-handed? team)) (short-handed-defenders? team))))

(defn calculate-substitute-value [substitute position team]
  (cond
    (= position ::player/center) (player/calculate-match-skill team substitute)
    (player/forward-position? position) (player/calculate-match-attack team substitute)
    :else (player/calculate-match-defense team substitute)))

(defn compare-substitute
  [[best best-value] candidate position team]
    (let [candidate-value (calculate-substitute-value candidate position team)]
       (cond
            (nil? best) [candidate candidate-value]
            (> (or candidate-value 0) (or best-value 0)) [candidate candidate-value]
            :else [best best-value])))

(defn pick-substitute [candidates position team]
  (println "pick-substitute" candidates position)
  (->> candidates
    (reduce #(compare-substitute %1 %2 position team) nil)
    first
    :id))

(defn collect-substitute-candidates [candidates excluded]
  (let [excluded? (fn excluded? [candidate]
                    (not-any? #(= % (:id candidate)) excluded))]
    (filter #(and (excluded? %) (player/dressed? %)) candidates)))

(defn pick-player-for-position [id field team position]
  (let [player (get-in team [:players id])]
    (println "pick-player-for-position" id player)
    (if (= (:status player) ::player/dressed)
      id
      (let [candidates (collect-substitute-candidates (vals (:players team)) field)]
        (pick-substitute candidates position team)))))

(defn pick-forwards-for-field [team field]
  (reduce #(assoc %1 %2 (pick-player-for-position (nth %1 %2) %1 team (get-position-by-index (+ 3 %2))))
    field
    (range (count field))))

(defn pick-defenders-for-field [team field]
  (reduce #(assoc %1 %2 (pick-player-for-position (nth %1 %2) %1 team (get-position-by-index (inc %2))))
    field
    (range (count field))))

(defn auto-field-nth [goalies defenders left-wings centers right-wings n]
  [(:id (first goalies))
   (map :id (take 2 (drop (* 2 n) defenders)))
   [(:id (nth left-wings n)) (:id (nth centers n)) (:id (nth right-wings n))]])

(defn auto-fields [team]
  (let [players (vals (:players team))
        goalies (->> players (filter player/goalie?) (sort-by :defense) reverse)
        defenders (->> players (filter player/defender?) (sort-by :defense) reverse)
        left-wings (->> players (filter player/left-wing?) (sort-by :attack) reverse)
        centers (->> players (filter player/center?) (sort-by :attack) reverse)
        right-wings (->> players (filter player/right-wing?) (sort-by :attack) reverse)]
    ;; FIXME: Handle left-, right-wings and center correctly
    [{:index 0 :shift-forwards 40 :shift-defenders 50 :players (auto-field-nth goalies defenders left-wings centers right-wings 0)}
     {:index 1 :shift-forwards 30 :shift-defenders 40 :players (auto-field-nth goalies defenders left-wings centers right-wings 1)}
     {:index 2 :shift-forwards 20 :shift-defenders 30 :players (auto-field-nth goalies defenders left-wings centers right-wings 2)}
     {:index 3 :shift-forwards 20 :players (auto-field-nth goalies defenders left-wings centers right-wings 3)}]))
