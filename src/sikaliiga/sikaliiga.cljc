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

(def *period-length* (* 20 60))
(def *game-length* (* 3 *period-length*))
(def *over-time-length* (* 5 60))

;; Following constants are calculated from Liiga season 2015-2016
;; NOTE: Multiply by two because we are halfing the probability with the posession mechanism
(def *mean-shots-per-sec* (* 2 (/ 40 *period-length*)))
(defn shot?
  ([attack defense rand]
  (< (rand)
     (+ (* (- attack defense) *mean-shots-per-sec*) *mean-shots-per-sec*)))
  ([attack defense]
    (shot? attack defense rand)))

(def *mean-block-probability* 0.12)
(defn blocked? [attack defense]
  (< (rand)
     (+ (* (- defense attack) *mean-block-probability*) *mean-block-probability*)))

(def *mean-miss-probability* 0.34)
(defn missed? [attack defense]
  (< (rand)
     (+ (* (- (* defense 0.5) attack) *mean-miss-probability*) *mean-miss-probability*)))

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

(defn add-sog [state sog-team player sog-against-team goalie]
  (-> state
      (update-in [:teams sog-team :players player :sog] inc)
      ;; FIXME: goalie might be nil, this might effect stats calculation
      (update-in [:teams sog-against-team :players goalie :sog-against] inc)
      (update :events #(conj % [:sog (:seconds state) sog-team player sog-against-team goalie]))))

;; TODO: assists
(defn add-goal [state goal-team player goal-against-team goalie]
  (-> state
      (update-in [:teams goal-team :players player :goals] inc)
      ;; FIXME: goalie might be nil, this might effect stats calculation
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

;; NOTE: One part of PP and SH skills come from the fact that defending team has
;; one player less than the other and skills are probably biased towards A and D
;; respectively. We should include PP and SH in calculating posession and shot
;; probabilities
(defn calculate-field-attack [team field]
  (->> (field/get-players field)
       (map #(get-in team [:players %]))
       (map #(calculate-player-attack team %))
       (reduce + 0)
       (#(/ % 5))))

(defn calculate-field-defense [team field]
  (->> (field/get-players field)
       (map #(get-in team [:players %]))
       (map #(calculate-player-defense team %))
       (reduce + 0)
       (#(/ % 5))))

(defn calculate-field-skill [team field]
  (->> (field/get-players field)
       (map #(get-in team [:players %]))
       (map #(calculate-player-skill team %))
       (reduce + 0)
       (#(/ % 5))))

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

(defn get-center [team field]
  (get-in team [:players (field/get-center field)]))

(defn face-off? [state]
  ;; TODO: In addition to this, face-off should be flagged on previous second
  ;; if there is a goal or a penalty, and by some random factor if there was a save/miss/block
  ;; (too specific?). In addition to this we perform some random factor to decide on face-off.
  (<= 0 (.indexOf [0 1200 2400 3600] (:seconds state))))

;; TODO: I need tests! All these functions have to take either [state r] or [state] that calls [state r] with default random generator
(defn simulate-face-off [state]
  (let [team-a (get-in state [:teams :home])
        team-b (get-in state [:teams :away])
        center-a (get-center team-a (:field team-a))
        center-b (get-center team-b (:field team-b))]
    ;; TODO: Should be affected by whole skill of centers, attack + defense
    ;; TODO: If we use a face-off flag from previous second, reset that flag.
    (if (< (calculate-player-skill team-a center-a) (rand (+ (calculate-player-skill team-a center-a) (calculate-player-skill team-b center-b))))
      (add-face-off state :home (:id center-a) :away (:id center-b))
      (add-face-off state :away (:id center-b) :home (:id center-a)))))

;; TODO: One idea I had was that posession should "stick", meaning that the team that has the posession
;; from last second should have higher probability to maintain the posession.
;; Also PP and SH should affect the probability for posession
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
    (if (= posession :home) :away :home)))

(defn get-posession-team [state]
  (get-in state [:teams (:posession state)]))

(defn get-non-posession-team [state]
  (get-in state [:teams (get-non-posession state)]))

(defn get-goalie [team field]
  (get-in team [:players (field/get-goalie field)]))

(defn get-blocker [team field]
  (get-in team [:players (rand-nth (field/get-defenders field))]))

;; TODO: Team tactics should affect the shooter (forwards vs defender)
(defn get-shooter [team field]
  (get-in team [:players (rand-nth (field/get-players field))]))

(defn simulate-goal [state shooter attacking-team defending-team]
  (let [goalie (get-goalie defending-team (:field defending-team))]
    (if (goal? (calculate-player-attack attacking-team shooter) (calculate-player-defense defending-team goalie))
        (add-goal state (:match-team attacking-team) (:id shooter) (:match-team defending-team) (:id goalie))
        (add-sog state (:match-team attacking-team) (:id shooter) (:match-team defending-team) (:id goalie)))))

(defn simulate-miss [state shooter attacking-team defending-team]
  (let [defense (calculate-field-defense defending-team (:field defending-team))]
    (if (missed? (calculate-player-attack attacking-team shooter) defense)
        (add-miss state (:match-team attacking-team) (:id shooter))
        (simulate-goal state shooter attacking-team defending-team))))

(defn simulate-block [state shooter attacking-team defending-team]
  (let [blocker (get-blocker defending-team (:field defending-team))]
    (if (blocked? (calculate-player-attack attacking-team shooter) (calculate-player-defense defending-team blocker))
        (add-block state (:match-team defending-team) (:id blocker) (:match-team attacking-team) (:id shooter))
        (simulate-miss state shooter attacking-team defending-team))))

(defn simulate-shot* [state attacking-team defending-team]
  (let [shooter (get-shooter attacking-team (:field attacking-team))
        shot-state (add-shot state (:match-team attacking-team) (:id shooter))]
    (simulate-block shot-state shooter attacking-team defending-team)))

;; TODO: PP and SH should affect the shot probability.
;; TODO: Team tactics should affect shot probability
(defn simulate-shot [state]
  (let [attacking-team (get-in state [:teams (:posession state)])
        defending-team (get-in state [:teams (get-non-posession state)])
        attack (calculate-field-attack attacking-team (:field attacking-team))
        defense (calculate-field-defense defending-team (:field defending-team))]
    (if (shot? attack defense)
      (simulate-shot* state attacking-team defending-team)
      state)))

(defn simulate-second [state]
  (-> state
      simulate-posession
      simulate-extras
      simulate-shot))

(defn prepare-player [match-team team player]
  (merge player
         {:team (:id team)
          ;; TODO: Utilize :match-team in data mapping
          :match-team match-team
          :attack (/ (:attack player) 100)
          :defense (/ (:defense player) 100)
          :fitness (/ (:fitness player) 100)
          :morale (/ (:morale player) 100)
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
          ;; TODO: Utilize :match-team in data mapping
          :match-team match-team
          :field (first (:fields team))
          :shots 0
          :blocked 0
          :missed 0
          :goals 0
          :goals-against 0
          :blocks 0}))

(defn prepare-state [home away]
  {:teams {:home (prepare-team :home home)
           :away (prepare-team :away away)}})

;; Team looked like this:
(defn simulate-game [home away]
  (let [state (prepare-state home away)
        seconds (range 3600)
        foo (reduce #(simulate-second (assoc %1 :seconds %2)) state seconds)]
    foo))

;; Starting to look good
(def team-a (team/make-test-team 50 70))
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
