(ns sikaliiga.simulation.match.penalties
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.simulation.match.specs :as specs]
            [sikaliiga.simulation.match.util :as mutil]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]
            [sikaliiga.field :as field]))

;;
;; Predicates

(def mean-minors-per-sec (/ 3.38 mutil/game-length))
(defn minor?
  ([rand]
   (< (rand) mean-minors-per-sec))
  ([]
   (minor? rand)))

(def mean-majors-per-sec (/ 0.18 mutil/game-length))
(defn major?
  ([rand]
   (< (rand) mean-majors-per-sec))
  ([]
   (major? rand)))

(def mean-match-penalties-per-sec (/ 0.09 mutil/game-length))
(defn match-penalty?
  ([rand]
   (< (rand) mean-match-penalties-per-sec))
  ([]
   (match-penalty? rand)))

;;
;; Reducers

(defn add-penalty [state match-team player-id length]
  (s/assert (s/cat :add-penalty/state ::specs/state :add-penalty/match-team keyword?
                   :add-penalty/player-id uuid?)
            [state match-team player-id])
  (-> state
      (assoc-in [:teams match-team :players player-id :status] ::player/penalty)
      (update-in [:teams match-team :penalty-box] #(assoc % player-id {:time (:seconds state) :length length}))
      (mutil/add-event :penalty match-team player-id length)))

(defn get-penalty-sitter [team field]
  (s/assert (s/cat :get-shooter/team ::specs/team :get-shooter/field ::field/field)
            [team field])
  ;; FIXME: Bias with character and defense attributes
  (get-in team [:players (util/rnd-nth (field/get-players field))]))

(defn mark-power-play [state team]
  (assoc-in state [:teams (:match-team team) :power-play?] (mutil/power-play? state team)))

(defn mark-short-handed [state team]
  (assoc-in state [:teams (:match-team team) :short-handed?] (mutil/short-handed? state team)))

;;
;; Simulation

(defn simulate-penalties* [state team]
  (s/assert (s/cat :simulate-penalties*/state ::specs/state
                   :simulate-penalties*/team ::specs/team)
            [state team])
  (cond
    (minor?) (add-penalty state (:match-team team) (:id (get-penalty-sitter team (:field team))) 120)
    (major?) (add-penalty state (:match-team team) (:id (get-penalty-sitter team (:field team))) 300)
    ;; Used to test pp/sh fields
    ; (and (= :home (:match-team team)) (= 3550 (:seconds state)))
    ;  (add-penalty state (:match-team team) (:id (get-penalty-sitter team (:field team))) 120)
    :else state))

(defn simulate-penalties [state]
  (-> state
      (simulate-penalties* (get-in state [:teams :home]))
      (simulate-penalties* (get-in state [:teams :away]))))

(defn release-penalty-sitter [state match-team player-id]
  (-> state
      (assoc-in [:teams match-team :players player-id :status] ::player/dressed)
      (update-in [:teams match-team :penalty-box] dissoc player-id)))

(defn simulate-penalty-release* [state team]
  ;; TODO: Release oldest minor penalty if opposing team made a goal second before and this team is short-handed
  (let [released (filter #(>= (:seconds state) (+ (:time (second %)) (:length (second %))))
                         (:penalty-box team))]
    (reduce #(release-penalty-sitter %1 (:match-team team) (first %2)) state released)))

(defn simulate-penalty-release [state]
  (-> state
      (simulate-penalty-release* (get-in state [:teams :home]))
      (simulate-penalty-release* (get-in state [:teams :away]))
      (mark-power-play (get-in state [:teams :home]))
      (mark-power-play (get-in state [:teams :away]))
      (mark-short-handed (get-in state [:teams :home]))
      (mark-short-handed (get-in state [:teams :away]))))
