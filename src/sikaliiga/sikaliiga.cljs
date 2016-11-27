(ns sikaliiga.sikaliiga
  (:require [clojure.spec :as s]
            [sikaliiga.util :as util]
            [sikaliiga.field :as field]
            [sikaliiga.player :as player]
            [sikaliiga.team :as team]))

;; Specs
{:teams {:home {} :visitor {}}}
{:players {} ; (map prepare-player (:players team))
          :team "uuid"
          :field nil
          :shots 0
          :blocked 0
          :missed 0
          :goals 0
          :goals-against 0
          :blocks 0}

(s/def :team/field map?)    ;; TODO

(s/def :player/team uuid?)
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
                       (s/keys :req-un [:team/players :team/field])))

(s/def ::seconds integer?)
(s/def ::home ::team)
(s/def ::visitor ::team)
(s/def ::teams (s/keys :req-un [::home ::visitor]))
(s/def ::state (s/keys :req-un [::seconds ::teams]))

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

(defn add-posession [state team-id]
  (assoc state :posession team-id))

(defn add-shot [state player]
  (update-in state [:teams (:team player) :players (:id player) :shots] inc))

(defn add-block [state player]
  (update-in state [:teams (:team player) :players (:id player) :blocked] inc))

(defn add-miss [state player]
  (update-in state [:teams (:team player) :players (:id player) :missed] inc))

(defn add-goal [state player]
  (update-in state [:teams (:team player) :players (:id player) :goals] inc))

(defn add-face-off [state winner loser]
  (-> state
      (assoc :posession (:team winner))
      (update-in [:teams (:team winner) :players (:id winner) :face-offs] inc)
      (update-in [:teams (:team loser) :players (:id loser) :face-offs] inc)
      (update-in [:teams (:team winner) :players (:id winner) :face-off-wins] inc)
      (update :events #(conj % [:face-off (:seconds state) (:id winner) (:id loser)]))))

(comment (defn simulate-shots [state]
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
            (-> state (add-shot :home)))))))))

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

(defn calculate-field-attack [team field]
  ;; TODO: Powerplay
  (->> (field/get-forwards field)
       (map #(get-in team [:players %]))
       (map :attack)    ;; TODO: Proper mapping of attack
       (reduce + 0)
       (#(/ % 3))))

(defn calculate-field-defense [team field]
  ;; TODO: Powerplay
  (->> (field/get-defenders field)
       (map #(get-in team [:players %]))
       (map :defense)
       (reduce + 0)
       (#(/ % 3))))

(defn calculate-field-skill [team field]
  (+ (calculate-field-defense team field) (calculate-field-attack team field)))

(defn calculate-field-goalie [team field]
  (or (:defense (get-in team [:players (first field)])) 0))

(defn prepare-field [team field]
  (assoc field :attack (calculate-field-attack team field)
               :defense (calculate-field-defense team field)
               :goalie (calculate-field-goalie team field)))

(defn face-off? [state]
  (contains? [0 1200 2400 3600] (:seconds state)))

(defn simulate-face-off [state]
  (let [center-a (get-in state [:teams :home :players (field/get-center (get-in state [:teams :home :field]))])
        center-b (get-in state [:teams :visitor :players (field/get-center (get-in state [:teams :visitor :field]))])]
    (if (< (:attack center-a) (rand (+ (:attack center-a) (:attack center-b))))
      (add-face-off state center-a center-b)
      (add-face-off state center-b center-a))))

(defn simulate-posession* [state]
  (let [skill-a (calculate-field-skill (get-in state [:teams :home]) (get-in state [:teams :home :field]))
        skill-b (calculate-field-skill (get-in state [:teams :visitor]) (get-in state [:teams :visitor :field]))]
    (if (< skill-a (rand (+ skill-a skill-b)))
      (add-posession state :home)
      (add-posession state :visitor))))

(defn simulate-posession [state]
  (if (face-off? state)
    (simulate-face-off state)
    (simulate-posession* state)))

(defn simulate-second [state]
  (-> state
      simulate-posession))

;; Team looked like this:
(defn simulate-game [home visitor]
  (let [state {:teams {:home home :visitor visitor}}
        seconds (range 3600)
        foo (reduce #(simulate-second (assoc %1 :seconds %2)) state seconds)]
    foo))

;; Starting to look good
(def team-a (team/make-test-team 50 75))
(def team-b (team/make-test-team 55 80))

(defn prepare-player [team player]
  (assoc player :team (:id team)
                :attack (/ (:attack player) 100)
                :defense (/ (:defense player) 100)
                :shots 0
                :blocked 0
                :missed 0
                :goals 0
                :goals-against 0
                :blocks 0))

(defn filter-player-by-status [status player]
  (= (:status player) player))

(defn prepare-team [team]
  (merge team
         ;; TODO: Filter out injured and banned players
         {:players (util/map-values #(prepare-player team %) (filter #(filter-player-by-status ::player/dressed %2) (:players team)))
          :field (first (:fields team))
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
