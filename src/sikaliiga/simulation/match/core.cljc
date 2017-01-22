(ns sikaliiga.simulation.match.core
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.simulation.match.specs :as specs]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]
            [sikaliiga.team :as team]
            [sikaliiga.simulation.match.util :as mutil]
            [sikaliiga.simulation.match.penalties :as penalties]
            [sikaliiga.simulation.match.shifts :as shifts]
            [sikaliiga.simulation.match.posession :as posession]
            [sikaliiga.simulation.match.shot :as shot]
            [sikaliiga.simulation.match.shootouts :as shootouts]))

(s/check-asserts false)

;; Simulating the second should roughly be smth like this:
;; shift?
;;   simulate-shift
;; face-off?
;;  posession = simulate-face-off
;;  posession = simulate-posession
;; body-check?
;;   do penalty?    ;; Should penalty and injury be parallel?
;;      injury?
;;   do penalty?    ;; Should penalty and injury be parallel?
;;      injury?
;; shot?
;;   blocked?
;;   missed?
;;   goal?
;; Part of these has to be done inline without external reducer

;; TODO: Make this testable and test
(defn simulate-second [state]
  (s/assert ::specs/state state)
  (-> state
      penalties/simulate-penalty-release
      shifts/simulate-shifts
      posession/simulate-posession
      penalties/simulate-penalties
      ;;extras/simulate-extras
      shot/simulate-shot))
      ;;attributes/simulate-attrs

;; TODO: Make this testable and test
(defn simulate-ot [state]
  (loop [state* (simulate-second state)]
    (if (and (< (:seconds state*) 3900)
             (mutil/tied? state*))
      (recur (simulate-second (update state* :seconds inc)))
      state*)))

;; TODO: Make this testable and test
(defn simulate-shootouts [state]
  (-> state
      (assoc :shootouts? true)
      (shootouts/simulate-shootouts)))

;; TODO: Make this testable and test
(defn simulate-match [state]
  (let [seconds (range (:seconds state) 3600)
        state* (reduce #(-> %1
                            (simulate-second)
                            (assoc :seconds (inc %2)))
                       state seconds)]
    (if (mutil/tied? state*)
      (let [state** (simulate-ot state*)]
        (if (mutil/tied? state**)
          (simulate-shootouts state**)
          state**))
      state*)))

(defn prepare-player-attack [team player]
  ;; TODO: Add potential home boost to attributes if (= :home (:match-team team))
  ;; TODO: Add potential away minus to attributes if (= :away (:match-team team))
  (+ (:attack player)
     (rand (* (- (:attack-potential player) (:attack player))
              (* 0.5 (+ (:morale player) (:fitness player)))))))

(defn prepare-player-defense [team player]
  ;; TODO: Add potential home boost to attributes if (= :home (:match-team team))
  ;; TODO: Add potential away minus to attributes if (= :away (:match-team team))
  (+ (:defense player)
     (rand (* (- (:defense-potential player) (:defense player))
              (* 0.5 (+ (:morale player) (:fitness player)))))))

(defn prepare-player [match-team team player]
  (merge player
         {:team (:id team)
          ;; TODO: Utilize :match-team in data mapping
          :match-team match-team
          :attack (prepare-player-attack team player)
          :original-attack (:attack player)
          :defense (prepare-player-defense team player)
          :original-defense (:defense player)
          :fitness (:fitness player)
          :morale (:morale player)
          :on-ice? false
          :toc 0
          :face-offs 0
          :face-off-wins 0
          :shots 0
          :blocked 0
          :missed 0
          :goals 0
          :goals-against 0
          :blocks 0}))

(defn prepare-players [match-team team]
  (->> (vals (:players team))
       (filter #(= (:status %) ::player/dressed))
       (map #(prepare-player match-team team %))
       (util/key-by :id)))

(defn prepare-team [match-team team]
  (merge team
         {:players (prepare-players match-team team)
          :match-team match-team
          :current-field-forwards nil
          :current-field-defenders nil
          :field [nil nil nil]     ;; Needs to be initialized like with field full of nil position vectors so changing players also create the same structure
          :penalty-box {}
          :shots 0
          :sog 0
          :blocked 0
          :missed 0
          :goals 0
          :goals-against 0
          :blocks 0}))

(defn prepare-state [home away]
  {:teams {:home (prepare-team :home home)
           :away (prepare-team :away away)}
   :seconds 0
   :events []})

;; Starting to look good
(def team-a (team/make-test-team "Team A" 50 70))
(def team-b (team/make-test-team "Team B" 55 80))

(def state (prepare-state team-a team-b))

(defn team-stats [state team]
  {:shots (count (filter #(and (= :shot (first %)) (= team (nth % 2))) (:events state)))
   :sog (count (filter #(and (= :sog (first %)) (= team (nth % 2))) (:events state)))
   :goals (count (filter #(and (= :goal (first %)) (= team (nth % 2))) (:events state)))
   :penalties (count (filter #(and (= :penalty (first %)) (= team (nth % 2))) (:events state)))
   :power-play-shots (count (filter #(and (= :shot (first %)) (= :power-play (last %)) (= team (nth % 2))) (:events state)))
   :power-play-sog (count (filter #(and (= :sog (first %)) (= :power-play (last %)) (= team (nth % 2))) (:events state)))
   :power-play-goals (count (filter #(and (= :goal (first %)) (= :power-play (last %)) (= team (nth % 2))) (:events state)))
   :short-handed-shots (count (filter #(and (= :shot (first %)) (= :short-handed (last %)) (= team (nth % 2))) (:events state)))
   :short-handed-sog (count (filter #(and (= :sog (first %)) (= :short-handed (last %)) (= team (nth % 2))) (:events state)))
   :short-handed-goals (count (filter #(and (= :goal (first %)) (= :short-handed (last %)) (= team (nth % 2))) (:events state)))})
