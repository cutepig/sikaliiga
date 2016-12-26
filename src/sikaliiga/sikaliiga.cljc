(ns sikaliiga.sikaliiga
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.util :as util]
            [sikaliiga.field :as field]
            [sikaliiga.player :as player]
            [sikaliiga.team :as team]))

; (s/check-asserts true)

;; Specs FIXME: Some are deprecated models
(s/def :player/team uuid?)  ;; TODO: Add this back in, temporarily removed because of tests dont def this
(s/def :player/shots integer?)
(s/def :player/blocked integer?)
(s/def :player/missed integer?)
(s/def :player/goals integer?)
(s/def :player/goals-against integer?)
(s/def :player/blocks integer?)
(s/def ::player (s/merge ::player/player
                         (s/keys :req-un [:player/shots :player/blocked :player/missed
                                          :player/goals :player/goals-against :player/blocks])))

(s/def :team/field ::field/field)
(s/def :team/players (s/map-of uuid? ::player))
(s/def :team/match-team #{:home :away})

(s/def ::team (s/merge ::team/team
                       (s/keys :req-un [:team/field :team/players :team/match-team
                                        :team/current-field-forwards :team/current-field-defenders])))

(s/def ::seconds integer?)
(s/def ::posession keyword?)
(s/def ::home ::team)
(s/def ::away ::team)
(s/def ::teams (s/keys :req-un [::home ::away]))
(s/def ::state (s/keys :req-un [::seconds ::teams]
                       :opt-un [::posession]))

;;
;; Simulation functions

(def period-length (* 20 60))
(def game-length (* 3 period-length))
(def over-time-length (* 5 60))

;; Following constants are calculated from Liiga season 2015-2016
;; NOTE: Multiply by two because we are halfing the probability with the posession mechanism
(def mean-shots-per-sec (* 2 (/ 40 game-length)))
(defn shot?
  ([attack defense rand]
    (< (rand)
      (+ (* (- attack defense) mean-shots-per-sec) mean-shots-per-sec)))
  ([attack defense]
    (shot? attack defense rand)))

(def mean-block-probability 0.12)
(defn blocked?
  ([attack defense rand]
    (< (rand)
       (+ (* (- defense attack) mean-block-probability) mean-block-probability)))
  ([attack defense]
    (blocked? attack defense rand)))

(def mean-miss-probability 0.34)
(defn missed?
  ([attack defense rand]
    (< (rand)
       (+ (* (- (* defense 0.5) attack) mean-miss-probability) mean-miss-probability)))
  ([attack defense]
    (missed? attack defense rand)))

;; TODO: Factor in D to affect shot quality?
(def mean-goal-probability 0.09)
(defn goal?
  ([attack goalie rand]
    (< (rand)
       (+ (* (- attack goalie) mean-goal-probability) mean-goal-probability)))
  ([attack goalie]
    (goal? attack goalie rand)))

(defn add-posession [state match-team]
  (s/assert (s/cat :add-posession/state ::state :add-posession/match-team keyword?) [state match-team])
  (assoc state :posession match-team))

(defn add-face-off [state winner-match-team winner-player-id loser-match-team loser-player-id]
  (s/assert (s/cat :add-face-off/state ::state :add-face-off/winner-match-team keyword?
                   :add-face-off/winner-player-id uuid? :add-face-off/loser-match-team keyword?
                   :add-face-off/loser-player-id uuid?)
    [state winner-match-team winner-player-id loser-match-team loser-player-id])
  (-> state
      (assoc :posession winner-match-team)
      (update-in [:teams winner-match-team :players winner-player-id :face-offs] inc)
      (update-in [:teams loser-match-team :players loser-player-id :face-offs] inc)
      (update-in [:teams winner-match-team :players winner-player-id :face-off-wins] inc)
      ;; FIXME: whats wrong with (:seconds state), it shows 4, 3, 2, 1 for the face offs
      (update :events #(conj % [:face-off (:seconds state) winner-player-id loser-player-id]))))

(defn add-shot [state match-team player-id]
  (s/assert (s/cat :add-shot/state ::state :add-shot/match-team keyword? :add-shot/player-id uuid?)
            [state match-team player-id])
  (-> state
      (update-in [:teams match-team :players player-id :shots] inc)
      (update :events #(conj % [:shot (:seconds state) match-team player-id]))))

(defn add-block [state blocking-match-team blocking-player-id blocked-match-team blocked-player-id]
  (s/assert (s/cat :add-block/state ::state :add-block/blocking-match-team keyword?
                   :add-block/blocking-player-id uuid? :add-block/blocked-match-team keyword?
                   :add-block/blocked-player-id uuid?)
    [state blocking-match-team blocking-player-id blocked-match-team blocked-player-id])
  (-> state
      (update-in [:teams blocking-match-team :players blocking-player-id :blocks] inc)
      (update-in [:teams blocked-match-team :players blocked-player-id :blocked] inc)))

(defn add-miss [state match-team shooter-id]
  (s/assert (s/cat :add-miss/state ::state :add-miss/match-team keyword? :add-miss/shooter-id uuid?)
    [state match-team shooter-id])
  (update-in state [:teams match-team :players shooter-id :missed] inc))

(defn add-sog [state sog-match-team shooter-id sog-against-match-team goalie-id]
  (s/assert (s/cat :add-sog/state ::state :add-sog/sog-match-team keyword? :add-sog/shooter-id uuid?
                   :add-sog/sog-against-match-team keyword? :add-sog/goalie-id uuid?)
    [state sog-match-team shooter-id sog-against-match-team goalie-id])
  (-> state
      (update-in [:teams sog-match-team :players shooter-id :sog] inc)
      ;; FIXME: goalie-id might be nil, this might effect stats calculation
      (update-in [:teams sog-against-match-team :players goalie-id :sog-against] inc)
      (update :events #(conj % [:sog (:seconds state) sog-match-team shooter-id sog-against-match-team goalie-id]))))

;; TODO: assists
(defn add-goal [state goal-match-team shooter-id goal-against-match-team goalie-id]
  (s/assert (s/cat :add-goal/state ::state :add-goal/goal-match-team keyword?
                   :add-goal/shooter-id uuid? :add-goal/goal-against-match-team keyword?
                   :add-goal/goalie-id uuid?)
    [state goal-match-team shooter-id goal-against-match-team goalie-id])
  (-> state
      (update-in [:teams goal-match-team :players shooter-id :goals] inc)
      ;; FIXME: goalie-id might be nil, this might effect stats calculation
      (update-in [:teams goal-against-match-team :players goalie-id :goals-against] inc)
      (update :events #(conj % [:goal (:seconds state) goal-match-team shooter-id goal-against-match-team goalie-id]))))

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

(defn get-goalie [team field]
  (s/assert (s/cat :get-goalie/team ::team :get-goalie/field ::field/field) [team field])
  (get-in team [:players (field/get-goalie field)]))

(defn get-blocker [team field]
  (s/assert (s/cat :get-blocker/team ::team :get-blocker/field ::field/field) [team field])
  (get-in team [:players (util/rnd-nth (field/get-defenders field))]))

;; TODO: Team tactics should affect the shooter (forwards vs defender)
(defn get-shooter [team field]
  ; (println "get-shooter" field)
  (s/assert (s/cat :get-shooter/team ::team :get-shooter/field ::field/field) [team field])
  ;; FIXME: When we finally get defenders shifts implemented, use get-players
  (comment (get-in team [:players (util/rnd-nth (field/get-players field))]))
  (get-in team [:players (util/rnd-nth (field/get-forwards field))]))

(defn get-center [team field]
  (s/assert (s/cat :get-center/team ::team :get-center/field ::field/field)
    [team field])
  (get-in team [:players (field/get-center field)]))

(defn power-play? [state team]
  ;; FIXME: We may have to be calculating penalt boxes for both teams
  ;; in order to know what is the real state, 5-4, 5-3, 4-3, 4-4, 3-3
  (:power-play? team))

(defn short-handed [state team]
  ;; FIXME: We may have to be calculating penalt boxes for both teams
  ;; in order to know what is the real state, 5-4, 5-3, 4-3, 4-4, 3-3
  (:short-handed? team))

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

;; FIXME: We may not even need this predicate, we can map this directly to field change
(defn shift-forwards? [state team]
  (or (util/period-start? (:seconds state))
      ;; no posession - should stop here
      (>= (:seconds state) (or (:next-shift-forwards team) 0))
      (and (:power-play? team) (not (power-play-forwards? team)))
      (and (:short-handed? team) (not (short-handed-forwards? team)))
      (and (not (:power-play? team)) (power-play-forwards? team))
      (and (not (:short-handed? team)) (short-handed-forwards? team))))

(defn shift-defenders? [state team]
  (or (util/period-start? (:seconds state))
      ;; no posession - should stop here
      (>= (:seconds state) (or (:next-shift-defenders team) 0))
      (and (:power-play? team) (not (power-play-defenders? team)))
      (and (:short-handed? team) (not (short-handed-defenders? team)))
      (and (not (:power-play? team)) (power-play-defenders? team))
      (and (not (:short-handed? team)) (short-handed-defenders? team))))

(defn shift-forwards* [state team idx]
  ; (println "shift-forwards*" idx (get-in state [:teams (:match-team team) :field]))
  (s/assert (s/cat :shift-forwards*/state ::state :shift-forwards*/team ::team :shift-forwards*/idx int?)
    [state team idx])
  (let [match-team (:match-team team)
        field (get-in team [:fields :forwards idx])]
    (s/assert (s/cat :shift-forwards*/match-team keyword? :shift-fowards*/field ::team/field)
      [match-team field])
    (-> state
        ;; TODO: Reset on-ice? for players in old field and set it for the new field
        (assoc-in [:teams match-team :field 2] (field/pick-forwards-for-field team (:players field)))
        (assoc-in [:teams match-team :current-field-forwards] idx)
        (assoc-in [:teams match-team :next-shift-forwards] (+ (:seconds state) (:shift-length field))))))

(defn shift-forwards [state team]
  (s/assert (s/cat :shift-forwards/state ::state :shift-forwards/team ::team) [state team])
  (cond
    ;; Always force shift to first field on period starts
    (util/period-start? (:seconds state))
      (shift-forwards* state team 0)
    ;; Never do shift is the team doesn't have posession
    ;; FIXME: Do some probability based rand here
    (not= (:posession state) (:match-team team))
      state
    ;; TODO: Test
    ;; See if the time for current shift is past due
    (>= (:seconds state) (or (:next-shift-forwards team) 0))
      (let [next-raw-idx (inc (:current-field-forwards team))
            ;; Choose the clamp range for next calculated field index
            ;; FIXME: We'd need to be aware of different short-handed constructions, 4-5, 3-5, 3-4
            next-idx (cond (:power-play? team) (util/mod-to-range next-raw-idx 4 5)
                           (:short-handed? team) (util/mod-to-range next-raw-idx 6 7)
                           :else (util/mod-to-range next-raw-idx 0 3))]
        (shift-forwards* state team next-idx))
    ;; FIXME: We'd need to be aware of different power-play constructions, 5-4, 5-3, 4-3
    (and (:power-play? team) (not (power-play-forwards? team)))
      ;; Power-play started, put on the first power-play field
      (shift-forwards* state team 4)
    ;; FIXME: We'd need to be aware of different power-play constructions, 5-4, 5-3, 4-3
    (and (:short-handed? team) (not (short-handed-forwards? team)))
      ;; Short-hand situation started, put on the first short-handed field
      (shift-forwards* state team 6)
    :else state))

(defn shift-defenders* [state team idx]
  ; (println "shift-defenders*" idx (get-in team [:fields :defenders :idx])
  ;   (get-in state [:teams (:match-team team) :field]))
  state)

(defn shift-defenders [state team]
  (comment "TODO")
  (cond
    (util/period-start? (:seconds state))
      (shift-defenders* state team 0)
    :else state))

(defn shift-goalie* [state team idx]
  ;; TODO: Be smarter, dressed? injured? etc..
  (let [goalie (nth (get-in team [:fields :goalies]) 0)]
    (update-in state [:teams (:match-team team) :field]
      #(assoc % 0 goalie))))

(defn shift-goalie [state team]
  (if (nil? (get-goalie team (:field team)))
    (shift-goalie* state team 0)
    state))

;; NOTE: One part of PP and SH skills come from the fact that defending team has
;; one player less than the other and skills are probably biased towards A and D
;; respectively. We should include PP and SH in calculating posession and shot
;; probabilities
(defn calculate-field-attack [team field]
  (->> (field/get-players field)
       (map #(get-in team [:players %]))
       (map #(player/calculate-match-attack team %))
       (reduce + 0)
       (#(/ % 5))))

(defn calculate-field-defense [team field]
  (->> (field/get-players field)
       (map #(get-in team [:players %]))
       (map #(player/calculate-match-defense team %))
       (reduce + 0)
       (#(/ % 5))))

(defn calculate-field-skill [team field]
  (->> (field/get-players field)
       (map #(get-in team [:players %]))
       (map #(player/calculate-match-skill team %))
       (reduce + 0)
       (#(/ % 5))))

(defn calculate-field-goalie [team field]
  (or (:defense (get-in team [:players (first field)])) 0))

(defn prepare-field [team field]
  (assoc field :attack (calculate-field-attack team field)
               :defense (calculate-field-defense team field)
               :goalie (calculate-field-goalie team field)))

(defn simulate-shifts [state]
  (s/assert map? state)
  ;; Shift happens on 0 1200 2400 and 3600 seconds and when shift counter is full.
  ;; But it is also affected by the posession from the last second; the team that
  ;; doesn't have the posession is less likely to shift.
  (->> (-> state
           (shift-forwards (get-in state [:teams :home]))
           (shift-defenders (get-in state [:teams :home]))
           (shift-goalie (get-in state [:teams :home]))
           (shift-forwards (get-in state [:teams :away]))
           (shift-defenders (get-in state [:teams :away]))
           (shift-goalie (get-in state [:teams :away])))
       (s/assert map?)))

(defn face-off? [state]
  (s/assert map? state)
  ;; TODO: In addition to this, face-off should be flagged on previous second
  ;; if there is a goal or a penalty, and by some random factor if there was a save/miss/block
  ;; (too specific?). In addition to this we perform some random factor to decide on face-off.
  (or (util/period-start? (:seconds state))
      (:face-off? state)))

;; TODO: I need tests! All these functions have to take either [state r] or [state] that calls [state r] with default random generator
(defn simulate-face-off [state]
  ; (println "simulate-face-off" (get-in state [:teams :home :field]))
  (s/assert map? state)
  (let [team-a (get-in state [:teams :home])
        team-b (get-in state [:teams :away])
        center-a (get-center team-a (:field team-a))
        center-b (get-center team-b (:field team-b))]
    (s/assert (s/cat :team-a map? :team-b map? :center-a map? :center-b map?)
      [team-a team-b center-a center-b])
    ;; TODO: Should be affected by whole skill of centers, attack + defense
    ;; TODO: If we use a face-off flag from previous second, reset that flag.
    (if (< (player/calculate-match-skill team-a center-a) (rand (+ (player/calculate-match-skill team-a center-a) (player/calculate-match-skill team-b center-b))))
        (add-face-off state :home (:id center-a) :away (:id center-b))
        (add-face-off state :away (:id center-b) :home (:id center-a)))))

;; TODO: One idea I had was that posession should "stick", meaning that the team that has the posession
;; from last second should have higher probability to maintain the posession.
;; Also PP and SH should affect the probability for posession
;; Ane also if team performed a shift this second there is a smaller chance to gain posession
(defn simulate-posession* [state]
  (s/assert map? state)
  (let [skill-a (calculate-field-skill (get-in state [:teams :home]) (get-in state [:teams :home :field]))
        skill-b (calculate-field-skill (get-in state [:teams :away]) (get-in state [:teams :away :field]))]
    (if (< skill-a (rand (+ skill-a skill-b)))
      (add-posession state :home)
      (add-posession state :away))))

(defn simulate-posession [state]
  (s/assert map? state)
  (if (face-off? state)
      ;; FIXME: Something is losing info here, it's proven that it's not this
      (simulate-face-off state)
      (simulate-posession* state)))

(defn simulate-extras [state]
  (s/assert map? state)
  state)

(defn get-posession [state]
  (s/assert map? state)
  (:posession state))

(defn get-non-posession [state]
  (s/assert map? state)
  (let [posession (:posession state)]
    (if (= posession :home) :away :home)))

(defn get-posession-team [state]
  (s/assert map? state)
  (get-in state [:teams (:posession state)]))

(defn get-non-posession-team [state]
  (s/assert map? state)
  (get-in state [:teams (get-non-posession state)]))

(defn simulate-goal [state shooter attacking-team defending-team]
  (s/assert (s/cat :simulate-goal/state ::state :simulate-goal/shooter ::player
                   :simulate-goal/attacking-team ::team :simulate-goal/defending-team ::team)
    [state shooter attacking-team defending-team])
  (let [goalie (get-goalie defending-team (:field defending-team))]
    (if (goal? (player/calculate-match-attack attacking-team shooter) (player/calculate-match-defense defending-team goalie))
        (add-goal state (:match-team attacking-team) (:id shooter) (:match-team defending-team) (:id goalie))
        (add-sog state (:match-team attacking-team) (:id shooter) (:match-team defending-team) (:id goalie)))))

(defn simulate-miss [state shooter attacking-team defending-team]
  (s/assert (s/cat :simulate-miss/state ::state :simulate-miss/shooter ::player
                   :simulate-miss/attacking-team ::team :simulate-miss/defending-team ::team)
    [state shooter attacking-team defending-team])
  (let [defense (calculate-field-defense defending-team (:field defending-team))]
    (if (missed? (player/calculate-match-attack attacking-team shooter) defense)
        (add-miss state (:match-team attacking-team) (:id shooter))
        (simulate-goal state shooter attacking-team defending-team))))

(defn simulate-block [state shooter attacking-team defending-team]
  (s/assert (s/cat :state ::state :shooter ::player :attacking-team ::team :defending-team ::team) [state shooter attacking-team defending-team])
  (let [blocker (get-blocker defending-team (:field defending-team))]
    (if (blocked? (player/calculate-match-attack attacking-team shooter) (player/calculate-match-defense defending-team blocker))
        (add-block state (:match-team defending-team) (:id blocker) (:match-team attacking-team) (:id shooter))
        (simulate-miss state shooter attacking-team defending-team))))

(defn simulate-shot* [state attacking-team defending-team]
  (s/assert (s/cat :state ::state :attacking-team ::team :defending-team ::team) [state attacking-team defending-team])
  (let [shooter (get-shooter attacking-team (:field attacking-team))
        shot-state (add-shot state (:match-team attacking-team) (:id shooter))]
    (simulate-block shot-state shooter attacking-team defending-team)))

;; TODO: PP and SH should affect the shot probability.
;; TODO: Team tactics should affect shot probability
(defn simulate-shot [state]
  (s/assert map? state)
  (let [attacking-team (get-in state [:teams (:posession state)])
        defending-team (get-in state [:teams (get-non-posession state)])
        attack (calculate-field-attack attacking-team (:field attacking-team))
        defense (calculate-field-defense defending-team (:field defending-team))]
    (if (shot? attack defense)
      (simulate-shot* state attacking-team defending-team)
      state)))

;; FIXME: Something is up with state updates, information seems to be getting lost
;; With each second
(defn simulate-second [state]
  (s/assert ::state state)
  (-> state
      ;; TODO: simulate-penalty-releases
      simulate-shifts
      simulate-posession
      simulate-extras
      simulate-shot))

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
          :defense (prepare-player-defense team player)
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
          :shots 0
          :blocked 0
          :missed 0
          :goals 0
          :goals-against 0
          :blocks 0}))

(defn prepare-state [home away]
  {:teams {:home (prepare-team :home home)
           :away (prepare-team :away away)}
   :events []})

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
