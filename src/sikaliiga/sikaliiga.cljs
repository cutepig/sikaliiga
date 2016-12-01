(ns sikaliiga.sikaliiga
  (:require [clojure.spec :as s]
            [sikaliiga.util :as util]
            [sikaliiga.field :as field]
            [sikaliiga.player :as player]
            [sikaliiga.team :as team]))

;; Specs
{:teams {:home {} :away {}}}
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
(s/def ::away ::team)
(s/def ::teams (s/keys :req-un [::home ::away]))
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
;; NOTE: Multiply by two because we are halfing the probability with the posession mechanism
(def *mean-shots-per-sec* (* 2 (/ 40 3600)))
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

(defn add-face-off [state winner-team winner-player loser-team loser-player]
  (-> state
      (assoc :posession winner-team)
      (update-in [:teams winner-team :players winner-player :face-offs] inc)
      (update-in [:teams loser-team :players loser-player :face-offs] inc)
      (update-in [:teams winner-team :players winner-player :face-off-wins] inc)
      ;; FIXME: whats wrong with (:seconds state), it shows 4, 3, 2, 1 for the face offs
      (update :events #(conj % [:face-off (:seconds state) winner-player loser-player]))))

(defn add-shot [state team player]
  (-> state
      (update-in [:teams team :players player :shots] inc)
      (update :events #(conj % [:shot (:seconds state) team player]))))

(defn add-block [state blocking-team blocking-player blocked-team blocked-player]
  (-> state
      (update-in [:teams blocking-team :players blocking-player :blocks] inc)
      (update-in [:teams blocked-team :players blocked-player :blocked] inc)))

(defn add-miss [state team player]
  (update-in state [:teams team :players player :missed] inc))

;; TODO: assists
(defn add-goal [state goal-team player goal-against-team goalie]
  (-> state
      (update-in [:teams goal-team :players player :goals] inc)
      (update-in [:teams goal-against-team :players goalie :goals-against] inc)
      (update :events #(conj % [:goal (:seconds state) goal-team player goal-against-team goalie]))))

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

(defn calculate-player-attack [team player]
  ;; FIXME: Proper mapping including team attributes, personal fitness and motivation
  (:attack player))

(defn calculate-player-defense [team player]
  ;; FIXME: Proper mapping including team attributes, personal fitness and motivation
  (:defense player))

(defn calculate-player-skill [team player]
  ;; FIXME: Proper mapping including team attributes, personal fitness and motivation
  (+ (calculate-player-attack team player)
     (calculate-player-defense team player)))

(defn calculate-field-attack [team field]
  ;; TODO: Powerplay
  ;; FIXME: This should include all field players divided by 5
  (->> (field/get-forwards field)
       (map #(get-in team [:players %]))
       (map #(calculate-player-attack team %))
       (reduce + 0)
       (#(/ % 3))))

(defn calculate-field-defense [team field]
  ;; TODO: Powerplay
  ;; FIXME: This should include all field players divided by 5
  (->> (field/get-defenders field)
       (map #(get-in team [:players %]))
       (map #(calculate-player-defense team %))
       (reduce + 0)
       (#(/ % 3))))

(defn calculate-field-skill [team field]
  ;; FIXME: This should include all field players divided by 5
  (+ (calculate-field-defense team field) (calculate-field-attack team field)))

(defn calculate-field-goalie [team field]
  (or (:defense (get-in team [:players (first field)])) 0))

(defn prepare-field [team field]
  (assoc field :attack (calculate-field-attack team field)
               :defense (calculate-field-defense team field)
               :goalie (calculate-field-goalie team field)))

(defn simulate-shift [state]
  ;; Shift happens on 0 1200 2400 and 3600 seconds and when shift counter is full.
  ;; But it is also affected by the posession from the last second; the team that
  ;; doesn't have the posession is less likely to shift.
  state)

(defn face-off? [state]
  ;; TODO: In addition to this, face-off should be flagged on previous second
  ;; if there is a goal or a penalty, and by some random factor if there was a save/miss/block
  ;; (too specific?). In addition to this we perform some random factor to decide on face-off.
  (<= 0 (.indexOf [0 1200 2400 3600] (:seconds state))))

;; TODO: I need tests! All these functions have to take either [state r] or [state] that calls [state r] with default random generator
(defn simulate-face-off [state]
  (let [center-a (get-in state [:teams :home :players (field/get-center (get-in state [:teams :home :field]))])
        center-b (get-in state [:teams :away :players (field/get-center (get-in state [:teams :away :field]))])]
    ;; TODO: Should be affected by whole skill of centers, attack + defense
    ;; TODO: If we use a face-off flag from previous second, reset that flag.
    (if (< (:attack center-a) (rand (+ (:attack center-a) (:attack center-b))))
      (add-face-off state :home (:id center-a) :away (:id center-b))
      (add-face-off state :away (:id center-b) :home (:id center-a)))))

(defn simulate-posession* [state]
  (let [skill-a (calculate-field-skill (get-in state [:teams :home]) (get-in state [:teams :home :field]))
        skill-b (calculate-field-skill (get-in state [:teams :away]) (get-in state [:teams :away :field]))]
    (if (< skill-a (rand (+ skill-a skill-b)))
      (add-posession state :home)
      (add-posession state :away))))

(defn simulate-posession [state]
  (if (face-off? state)
    (simulate-face-off state)
    (simulate-posession* state)))

(defn simulate-extras [state]
  state)

(defn get-posession [state]
  (:posession state))

(defn get-non-posession [state]
  (let [posession (:posession state)]
    (first (filter #(not= posession %) (keys (:teams state))))))

(defn get-posession-team [state]
  (get-in state [:teams (:posession state)]))

(defn get-non-posession-team [state]
  (get-in state [:teams (get-non-posession state)]))

(defn get-goalie [team field]
  (get-in team [:players (field/get-goalie field)]))

(defn get-blocker [team field]
  (get-in team [:players (rand-nth (field/get-defenders field))]))

(defn get-shooter [team field]
  (get-in team [:players (rand-nth (field/get-players field))]))

(defn simulate-shot [state]
  (let [posession (:posession state)
        non-posession (get-non-posession state)
        attacking-team (get-in state [:teams posession])
        defending-team (get-in state [:teams non-posession])
        field-attack (calculate-field-attack attacking-team (:field attacking-team))
        field-defense (calculate-field-defense defending-team (:field defending-team))]
    (if (shot? field-attack field-defense)
      (let [shooter (get-shooter attacking-team (:field attacking-team))
            shot-state (add-shot state posession (:id shooter))
            blocker (get-blocker defending-team (:field defending-team))
            goalie (get-goalie defending-team (:field defending-team))]
        (if (blocked? (calculate-player-attack attacking-team shooter) (calculate-player-defense defending-team blocker))
          (add-block shot-state non-posession (:id blocker) posession (:id shooter))
          (if (missed? (calculate-player-attack attacking-team shooter) field-defense)
            (add-miss shot-state posession (:id shooter))
            (if (goal? (calculate-player-attack attacking-team shooter) (calculate-player-defense defending-team goalie))
              (add-goal shot-state posession (:id shooter) non-posession (:id goalie))
              shot-state))))
      state)))

(defn simulate-second [state]
  (-> state
      simulate-posession
      simulate-extras
      simulate-shot))

(defn prepare-player [team player]
  (assoc player :team (:id team)
                :attack (/ (:attack player) 100)
                :defense (/ (:defense player) 100)
                :fitness (/ (:fitness player) 100)
                :morale (/ (:morale player) 100)
                :shots 0
                :blocked 0
                :missed 0
                :goals 0
                :goals-against 0
                :blocks 0))

(defn prepare-players [team]
  (->> (vals (:players team))
       (filter #(= (:status %) ::player/dressed))
       (map #(prepare-player team %))
       (util/key-by :id)))

(defn prepare-team [team]
  (merge team
         ;; TODO: Should team store :home or :away which would simplify data mapping?
         {:players (prepare-players team)
          :field (first (:fields team))
          :shots 0
          :blocked 0
          :missed 0
          :goals 0
          :goals-against 0
          :blocks 0}))

(defn prepare-state [home visitor]
  {:teams {:home (prepare-team home)
           :away (prepare-team visitor)}})

;; Team looked like this:
(defn simulate-game [home visitor]
  (let [state (prepare-state home visitor)
        seconds (range 3600)
        foo (reduce #(simulate-second (assoc %1 :seconds %2)) state seconds)]
    foo))

;; Starting to look good
(def team-a (team/make-test-team 50 75))
(def team-b (team/make-test-team 55 80))

(def state (prepare-state team-a team-b))

(comment
  (js/console.log (simulate-game team-a team-b)))

;; In-game fields shall look like this
{:players []
 :shift-forwards 40
 :index-forwards 0
 :next-shift-forwards 40
 :shift-defenders 50
 :index-defenders 0
 :next-shift-defenders 50}
