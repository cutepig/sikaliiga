(ns sikaliiga.simulation.match.posession
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.simulation.match.specs :as specs]
            [sikaliiga.simulation.match.util :as mutil]
            [sikaliiga.player :as player]
            [sikaliiga.field :as field]))

;;
;; Predicates

(def mean-face-offs-per-sec (/ 30 mutil/game-length))

(defn face-off?
  ([state rand]
   (s/assert ::specs/state state)
    ;; TODO: In addition to this, face-off should be flagged on previous second
    ;; if there is a ~~goal or a~~ penalty, and by some random factor if there was a save/miss/block
    ;; (too specific?). In addition to this we perform some random factor to decide on face-off.
   (or (mutil/period-start? (:seconds state))
       (= (get-in state [:teams :home :last-goal]) (dec (:seconds state)))
       (= (get-in state [:teams :away :last-goal]) (dec (:seconds state)))
       (< (rand) mean-face-offs-per-sec)))
  ([state]
   (face-off? state rand)))


;;
;; Reducers

(defn add-posession [state match-team]
  (s/assert (s/cat :add-posession/state ::specs/state :add-posession/match-team keyword?)
            [state match-team])
  (assoc state :posession match-team))

(defn add-face-off [state winner-match-team winner-player-id loser-match-team loser-player-id]
  (s/assert (s/cat :add-face-off/state ::specs/state :add-face-off/winner-match-team keyword?
                   :add-face-off/winner-player-id uuid? :add-face-off/loser-match-team keyword?
                   :add-face-off/loser-player-id uuid?)
    [state winner-match-team winner-player-id loser-match-team loser-player-id])
  (-> state
      (assoc :posession winner-match-team)
      (update-in [:teams winner-match-team :players winner-player-id :face-offs] inc)
      (update-in [:teams loser-match-team :players loser-player-id :face-offs] inc)
      (update-in [:teams winner-match-team :players winner-player-id :face-off-wins] inc)
      (mutil/add-event :face-off winner-match-team winner-player-id loser-match-team loser-player-id)))

;;
;; Utils

(defn get-center [team field]
  (s/assert (s/cat :get-center/team ::specs/team :get-center/field ::field/field)
            [team field])
  (get-in team [:players (field/get-center field)]))

;; TODO: I need tests! All these functions have to take either [state r] or [state] that calls [state r] with default random generator
(defn simulate-face-off [state]
  (s/assert ::specs/state state)
  (let [team-a (get-in state [:teams :home])
        team-b (get-in state [:teams :away])
        center-a (get-center team-a (:field team-a))
        center-b (get-center team-b (:field team-b))]
    (s/assert (s/cat :team-a map? :team-b map? :center-a map? :center-b map?)
      [team-a team-b center-a center-b])
    ;; TODO: Should be affected by whole skill of centers, attack + defense
    ;; TODO: If we use a face-off flag from previous second, reset that flag.
    (if (< (rand (+ (player/calculate-match-skill team-a center-a)
                    (player/calculate-match-skill team-b center-b)))
           (player/calculate-match-skill team-a center-a))
        (add-face-off state :home (:id center-a) :away (:id center-b))
        (add-face-off state :away (:id center-b) :home (:id center-a)))))

;; TODO: PP and SH should affect the probability for posession
;; Ane also if team performed a shift this second there is a smaller chance to gain posession
(defn simulate-posession* [state]
  (s/assert ::specs/state state)
  (let [[multiplier-a multiplier-b] (cond (nil? (:posession state)) [1 1]
                                          (= :home (:posession state)) [2 0.5]
                                          :else [0.5 2])
        skill-a (* multiplier-a (mutil/calculate-field-skill
                                  (get-in state [:teams :home])
                                  (get-in state [:teams :home :field])))
        skill-b (* multiplier-b (mutil/calculate-field-skill
                                  (get-in state [:teams :away])
                                  (get-in state [:teams :away :field])))]
    (if (< (rand (+ skill-a skill-b)) skill-a)
      (add-posession state :home)
      (add-posession state :away))))

(defn simulate-posession [state]
  (s/assert map? state)
  (if (face-off? state)
      (simulate-face-off state)
      (simulate-posession* state)))
