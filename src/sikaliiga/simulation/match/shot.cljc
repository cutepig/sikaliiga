(ns sikaliiga.simulation.match.shot
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.simulation.match.specs :as specs]
            [sikaliiga.simulation.match.util :as mutil]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]
            [sikaliiga.field :as field]))

;;
;; Predicates

;; Following constants are calculated from Liiga season 2015-2016
;; NOTE: Multiply by two because we are halfing the probability with the posession mechanism
(def mean-shots-per-sec (* 2 (/ 40 mutil/game-length)))
(defn shot?
  ([attack defense rand]
   (< (rand)
     (+ (* (- attack defense) mean-shots-per-sec) mean-shots-per-sec)))
  ([attack defense]
   (shot? attack defense rand)))

(def mean-block-probability (* 0.12 1))
(defn blocked?
  ([attack defense rand]
   (< (rand)
      (+ (* (- defense attack) mean-block-probability) mean-block-probability)))
  ([attack defense]
   (blocked? attack defense rand)))

(def mean-miss-probability (* 0.34 1))
(defn missed?
  ([attack defense rand]
   (< (rand)
      (+ (* (- (* defense 0.5) attack) mean-miss-probability) mean-miss-probability)))
  ([attack defense]
   (missed? attack defense rand)))

;; TODO: Factor in D to affect shot quality?
(def mean-goal-probability (* 0.09 1))
(defn goal?
  ([attack goalie rand]
   (< (rand)
      (+ (* (- attack goalie) mean-goal-probability) mean-goal-probability)))
  ([attack goalie]
   (goal? attack goalie rand)))

;;
;; Reducers

(defn add-shot [state match-team player-id]
  (s/assert (s/cat :add-shot/state ::specs/state :add-shot/match-team keyword?
                   :add-shot/player-id uuid?)
            [state match-team player-id])
  (let [type (cond (get-in state [:teams match-team :power-play?]) :power-play
                   (get-in state [:teams match-team :short-handed?]) :short-handed
                   :else nil)]
    (-> state
        (assoc-in [:teams match-team :last-shot] (:seconds state))
        (update-in [:teams match-team :players player-id :shots] inc)
        (update-in [:teams match-team :shots] inc)
        (mutil/add-event :shot match-team player-id type))))

(defn add-block [state blocking-match-team blocking-player-id blocked-match-team blocked-player-id]
  (s/assert (s/cat :add-block/state ::specs/state :add-block/blocking-match-team keyword?
                   :add-block/blocking-player-id uuid? :add-block/blocked-match-team keyword?
                   :add-block/blocked-player-id uuid?)
    [state blocking-match-team blocking-player-id blocked-match-team blocked-player-id])
  (-> state
      (update-in [:teams blocking-match-team :players blocking-player-id :blocks] inc)
      (update-in [:teams blocked-match-team :players blocked-player-id :blocked] inc)
      (update-in [:teams blocked-match-team :blocked] inc)))

(defn add-miss [state match-team shooter-id]
  (s/assert (s/cat :add-miss/state ::specs/state :add-miss/match-team keyword?
                   :add-miss/shooter-id uuid?)
    [state match-team shooter-id])
  (-> state
      (update-in [:teams match-team :players shooter-id :missed] inc)
      (update-in [:teams match-team :missed] inc)))

(defn add-sog [state sog-match-team shooter-id sog-against-match-team goalie-id]
  (s/assert (s/cat :add-sog/state ::specs/state :add-sog/sog-match-team keyword?
                   :add-sog/shooter-id uuid? :add-sog/sog-against-match-team keyword?
                   :add-sog/goalie-id uuid?)
    [state sog-match-team shooter-id sog-against-match-team goalie-id])
  (let [type (cond (get-in state [:teams sog-match-team :power-play?]) :power-play
                   (get-in state [:teams sog-match-team :short-handed?]) :short-handed
                   :else nil)]
    (-> state
        (update-in [:teams sog-match-team :players shooter-id :sog] inc)
        (update-in [:teams sog-match-team :sog] inc)
        ;; FIXME: goalie-id might be nil, this might effect stats calculation
        (update-in [:teams sog-against-match-team :players goalie-id :sog-against] inc)
        (mutil/add-event :sog sog-match-team shooter-id sog-against-match-team goalie-id type))))

;; TODO: assists
(defn add-goal [state goal-match-team shooter-id goal-against-match-team goalie-id]
  (s/assert (s/cat :add-goal/state ::specs/state :add-goal/goal-match-team keyword?
                   :add-goal/shooter-id uuid? :add-goal/goal-against-match-team keyword?
                   :add-goal/goalie-id uuid?)
    [state goal-match-team shooter-id goal-against-match-team goalie-id])
  (let [type (cond (get-in state [:teams goal-match-team :power-play?]) :power-play
                   (get-in state [:teams goal-match-team :short-handed?]) :short-handed
                   :else nil)]
    (-> state
        (assoc-in [:teams goal-match-team :last-goal] (:seconds state))
        (update-in [:teams goal-match-team :goals] inc)
        (update-in [:teams goal-against-match-team :goals-against] inc)
        ;; FIXME: Doesn't seem to be getting through?
        (update-in [:teams goal-match-team :players shooter-id :goals] inc)
        ;; FIXME: goalie-id might be nil, this might effect stats calculation
        (update-in [:teams goal-against-match-team :players goalie-id :goals-against] inc)
        (mutil/add-event :goal goal-match-team shooter-id goal-against-match-team goalie-id type))))

;;
;; Utils

(defn get-blocker [team field]
  (get-in team [:players (util/rnd-nth (field/get-defenders field))]))

;; TODO: Team tactics should affect the shooter (forwards vs defender)
(defn get-shooter [team field]
  ;; FIXME: Bias towards forwards
  ;; FIXME: Bias offset based on team tactics
  (get-in team [:players (util/rnd-nth (field/get-players field))]))

;;
;; Simulation

;; TODO: Should PP and SH affect goal probability?
(defn simulate-goal [state shooter attacking-team defending-team]
  (s/assert (s/cat :simulate-goal/state ::specs/state
                   :simulate-goal/shooter ::specs/player
                   :simulate-goal/attacking-team ::specs/team
                   :simulate-goal/defending-team ::specs/team)
            [state shooter attacking-team defending-team])
  (let [goalie (mutil/get-goalie defending-team (:field defending-team))]
    (if (goal? (player/calculate-match-attack attacking-team shooter) (player/calculate-match-defense defending-team goalie))
        (add-goal state (:match-team attacking-team) (:id shooter) (:match-team defending-team) (:id goalie))
        (add-sog state (:match-team attacking-team) (:id shooter) (:match-team defending-team) (:id goalie)))))

(defn simulate-miss [state shooter attacking-team defending-team]
  (s/assert (s/cat :simulate-miss/state ::specs/state
                   :simulate-miss/shooter ::specs/player
                   :simulate-miss/attacking-team ::specs/team
                   :simulate-miss/defending-team ::specs/team)
            [state shooter attacking-team defending-team])
  (let [defense (mutil/calculate-field-defense defending-team (:field defending-team))]
    (if (missed? (player/calculate-match-attack attacking-team shooter) defense)
        (add-miss state (:match-team attacking-team) (:id shooter))
        (simulate-goal state shooter attacking-team defending-team))))

;; TODO: Should PP and SH affect block probability?
(defn simulate-block [state shooter attacking-team defending-team]
  (s/assert (s/cat :simulate-block/state ::specs/state
                   :simulate-block/shooter ::specs/player
                   :simulate-block/attacking-team ::specs/team
                   :simulate-block/defending-team ::specs/team)
            [state shooter attacking-team defending-team])
  (let [blocker (get-blocker defending-team (:field defending-team))]
    (if (blocked? (player/calculate-match-attack attacking-team shooter)
                  (player/calculate-match-defense defending-team blocker))
        (add-block state (:match-team defending-team) (:id blocker)
                   (:match-team attacking-team) (:id shooter))
        (simulate-miss state shooter attacking-team defending-team))))

(defn simulate-shot* [state attacking-team defending-team]
  (s/assert (s/cat :simulate-shot*/state ::specs/state
                   :simulate-shot*/attacking-team ::specs/team
                   :simulate-shot*/defending-team ::specs/team)
            [state attacking-team defending-team])
  (let [shooter (get-shooter attacking-team (:field attacking-team))
        shot-state (add-shot state (:match-team attacking-team) (:id shooter))]
    (simulate-block shot-state shooter attacking-team defending-team)))

;; TODO: PP and SH should affect the shot probability.
;; TODO: Team tactics should affect shot probability
(defn simulate-shot [state]
  (s/assert ::specs/state state)
  (let [attacking-team (get-in state [:teams (:posession state)])
        defending-team (get-in state [:teams (mutil/get-non-posession state)])
        attack (mutil/calculate-field-attack attacking-team (:field attacking-team))
        defense (mutil/calculate-field-defense defending-team (:field defending-team))]
    (if (and (< (+ 5 (:last-shot defending-team)) (:seconds state))
             (shot? attack defense))
      (simulate-shot* state attacking-team defending-team)
      state)))
