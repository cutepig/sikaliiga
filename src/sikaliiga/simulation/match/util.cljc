(ns sikaliiga.simulation.match.util
  (:require [sikaliiga.player :as player]
            [sikaliiga.field :as field]))

(def period-length (* 20 60))
(def game-length (* 3 period-length))
(def over-time-length (* 5 60))

(defn add-event [state & args]
  (update-in state [:events] #(conj % (apply vector (first args) (:seconds state) (rest args)))))

(defonce opposing-match-team {:home :away :away :home})

(defn period-start? [seconds]
  (some? (#{0 1200 2400 3600} seconds)))

(defn get-goalie [team field]
  (get-in team [:players (field/get-goalie field)]))

(defn tied? [state]
  (= (get-in state [:teams :home :goals])
     (get-in state [:teams :away :goals])))

(defn power-play? [state team]
  (let [other (get-in state [:teams (opposing-match-team (:match-team team))])]
    (< (min 2 (count (:penalty-box team)))
       (min 2 (count (:penalty-box other))))))

(defn short-handed? [state team]
  (let [other (get-in state [:teams (opposing-match-team (:match-team team))])]
    (> (min 2 (count (:penalty-box team)))
       (min 2 (count (:penalty-box other))))))

;; NOTE: One part of PP and SH skills come from the fact that defending team has
;; one player less than the other and skills are probably biased towards A and D
;; respectively. We should include PP and SH in calculating posession and shot
;; probabilities. Also bias offsets should be affected by team pp and sh
;; coaching attributes?

;; FIXME: These are totally arbitrary
(def power-play-multiplier (/ 1.25 1))
(def short-handed-multiplier (/ 1 1.25))

(defn calculate-field-attack [team field]
  (->> (field/get-players field)
       (map #(get-in team [:players %]))
       (map #(player/calculate-match-attack team %))
       (reduce + 0)
       (* (cond (:power-play? team) power-play-multiplier
                (:short-handed? team) short-handed-multiplier
                :else 1))
       (* 0.2)))

(defn calculate-field-defense [team field]
  (->> (field/get-players field)
       (map #(get-in team [:players %]))
       (map #(player/calculate-match-defense team %))
       (reduce + 0)
       (* (cond (:power-play? team) power-play-multiplier
                (:short-handed? team) short-handed-multiplier
                :else 1))
       (* 0.2)))

(defn calculate-field-skill [team field]
  (->> (field/get-players field)
       (map #(get-in team [:players %]))
       (map #(player/calculate-match-skill team %))
       (reduce + 0)
       (* (cond (:power-play? team) power-play-multiplier
                (:short-handed? team) short-handed-multiplier
                :else 1))
       (* 0.2)))

(defn calculate-field-goalie [team field]
  (or (:defense (get-in team [:players (first field)])) 0))

;; FIXME: Should these be in `sikaliiga.simulation.match.posession`?

(defn get-posession [state]
  (:posession state))

(defn get-non-posession [state]
  (let [posession (:posession state)]
    (if (= posession :home) :away :home)))

(defn get-posession-team [state]
  (get-in state [:teams (:posession state)]))

(defn get-non-posession-team [state]
  (get-in state [:teams (get-non-posession state)]))
