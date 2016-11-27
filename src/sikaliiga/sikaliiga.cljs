(ns sikaliiga.sikaliiga
  (:require [clojure.spec :as s]
            [sikaliiga.util :as util]
            [sikaliiga.field :as field]
            [sikaliiga.player :as player]
            [sikaliiga.team :as team]))

;; Specs
{:teams {:home {} :visitor {}}}
{:players {} ; (map prepare-player (:players team))
          :field nil
          :shots 0
          :blocked 0
          :missed 0
          :goals 0
          :goals-against 0
          :blocks 0}

(s/def :player/field (s/nilable integer?))
(s/def :player/shots integer?)
(s/def :player/blocked integer?)
(s/def :player/missed integer?)
(s/def :player/goals integer?)
(s/def :player/goals-against integer?)
(s/def :player/blocks integer?)
(s/def ::player (s/merge ::player/player
                         (s/keys :req-un [:player/field :player/shots :player/blocked :player/missed
                                          :player/goals :player/goals-against :player/blocks])))

(s/def :team/players (s/coll-of ::player :kind vector?))

(s/def ::team (s/merge ::team/team
                       (s/keys :req-un [:team/players])))

(s/def ::home ::team)
(s/def ::visitor ::team)
(s/def ::teams (s/keys :req-un [::home ::visitor]))
(s/def ::state (s/keys :req-un [:state/teams]))

;;
;; Simulation functions

(defn power-play? [] (= 0 (rand-int 5)))

;; NOTE: This old version had power-play affecting the situation
;; TODO: Port this back to the new version
;; TODO: Also add in short-handed
(defn -shot? [a b power-play?]
  (if power-play?
    (> (rand (:power-play a)) (rand (:short-handed b)))
    (> (rand (:attack a)) (rand (:defense b)))))

(defn -goal? [a b power-play?]
  (if power-play?
    (> (rand (:power-play a)) (+ (rand (* 30 (:goalie b))) (* 15 (:goalie b))))
    (> (rand (:attack a)) (+ (rand (:goalie b)) (/ (:defense b) 3)))))

;; TODO: Tweak this constant
(def *mean-shots-per-sec* (/ 40 3600))
(defn shot? [attack defense]
  (< (rand)
     (+ (* (- attack defense) *mean-shots-per-sec*) *mean-shots-per-sec*)))

;; TODO: Tweak this constant
(def *mean-block-probability* 0.2)
(defn blocked? [attack defense]
  (< (rand)
     (+ (* (- defense attack) *mean-block-probability*) *mean-block-probability*)))

;; TODO: Tweak this constant
(def *mean-miss-probability* 0.2)
(defn missed? [attack defense]
  (< (rand)
     (+ (* (- (* defense 0.5) attack) *mean-miss-probability*) *mean-miss-probability*)))

;; TODO: Tweak this constant
;; TODO: Factor in D to affect shot quality?
(def *mean-goal-probability* 0.09)
(defn goal? [attack goalie]
  (< (rand)
     (+ (* (- attack goalie) *mean-goal-probability*) *mean-goal-probability*)))

(defmulti reducer (fn [_ [type]] type))

; (defmethod reducer ::shot [state [_ team-id other-id player-id]]
;   (player/by-id (update-in state [:teams team-id :players player-id]) player-id))

(defmethod reducer ::block [state [_ team-id other-id player-id]])
(defmethod reducer ::miss [state [_ team-id player-id]])
(defmethod reducer ::goal [state [_ team-id other-id player-id assist-ids]])

(defn add-shot [state team]
  (update-in state [:teams team :shots] inc))

(defn add-block-against [state team]
  (update-in state [:teams team :blocked] inc))

(defn add-block [state team]
  (update-in state [:teams team :blocks] inc))

(defn add-miss [state team]
  (update-in state [:teams team :missed] inc))

(defn add-goal [state team]
  (update-in state [:teams team :goals] inc))

(defn add-goal-against [state team]
  (update-in state [:teams team :goals-against] inc))

(defn simulate-shots [state]
  (let [home (get-in state [:teams :home])
        visitor (get-in state [:teams :visitor])]
    ;; Shot should be affected by whole field A D
    (if (not (shot? (:attack home) (:defense visitor)))
      state
      ;; Block should be affected by shooter A (?) and opponent field D
      ;; On the other hand field A determines the quality of the shot
      (if (blocked? (:attack home) (:defense visitor))
        ;; Initial idea was to return state and list of events which is still viable:
        ;; [state [add-shot team] [add-block-against team] [add-block opponent]]
        ;; Just went with threading to macro to get immediate result
        ;; Returning list of events allows us to give important meta-data to show full results
        (-> state (add-shot :home) (add-block-against :home) (add-block :visitor))
        ;; Miss should be affected by shooter A
        (if (missed? (:attack home) (:defense visitor))
          (-> state (add-shot :home) (add-miss :home))
          ;; Goal should be affected by shooter A and opponent G
          (if (goal? (:attack home) (:goalie visitor))
            (-> state (add-shot :home) (add-goal :home) (add-goal-against :visitor))
            (-> state (add-shot :home))))))))

;; Simulating the second should roughly be smth like this:
;; shift?
;;   simulate-shift
;; face-off?
;;  posession = simulate-face-off
;;  posession = simulate-posession
;; hit?
;;   do penalty?    ;; Should penalty and injury be parallel?
;;      injury?
;;   do penalty?    ;; Should penalty and injury be parallel?
;;      injury?
;; shot?
;;   blocked?
;;   missed?
;;   goal?

;; FIXME: Why isn't the state updated?!
(defn simulate-second [state sec]
  (simulate-shots state))

;; Team looked like this:
(defn simulate-game [home visitor]
  (let [state {:teams {:home home :visitor visitor}}
        seconds (range 3600)
        foo (reduce simulate-second state seconds)]
    foo))

;; Starting to look good
(def team-a (team/make-test-team 50 75))
(def team-b (team/make-test-team 55 80))

(defn prepare-player [player]
  (println "prepare-player" player)
  (assoc player :attack (/ (:attack player) 100)
                :defense (/ (:defense player) 100)))

(defn filter-player-by-status [status player]
  (= (:status player) player))

(defn prepare-team [team]
  (merge team
         ;; TODO: Filter out injured and banned players
         {:players (util/map-values prepare-player (filter #(filter-player-by-status :playing (second %)) (:players team)))
          :field nil
          :shots 0
          :blocked 0
          :missed 0
          :goals 0
          :goals-against 0
          :blocks 0}))

(defn prepare-state [home visitor]
  {:teams {:home (prepare-team home)
           :visitor (prepare-team visitor)}})

(def state (prepare-state team-a team-b))

(simulate-game (prepare-team team-a) (prepare-team team-b))

;; In-game fields shall look like this
{:players []
 :shift-forwards 40
 :index-forwards 0
 :next-shift-forwards 40
 :shift-defenders 50
 :index-defenders 0
 :next-shift-defenders 50}
