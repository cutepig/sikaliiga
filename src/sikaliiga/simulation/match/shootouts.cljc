(ns sikaliiga.simulation.match.shootouts
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.simulation.match.specs :as specs]
            [sikaliiga.simulation.match.util :as mutil]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]))

(defn add-shootout-goal [state round shooter-match-team shooter-id goalie-match-team goalie-id]
  (-> state
      (update-in [:teams shooter-match-team :shootouts] inc)
      (mutil/add-event :shootout-goal round shooter-match-team shooter-id goalie-match-team goalie-id)))

(defn add-shootout-miss [state round shooter-match-team shooter-id goalie-match-team goalie-id]
  (-> state
      (mutil/add-event :shootout-miss round shooter-match-team shooter-id goalie-match-team goalie-id)))

(defn goal?
  ([attack goalie rand]
   (< (rand (+ attack goalie)) attack))
  ([attack goalie]
   (goal? attack goalie rand)))

(defn shootouts-tied? [state]
  (= (get-in state [:teams :home :shootouts])
     (get-in state [:teams :away :shootouts])))

(defn shooter? [player]
  (and (player/dressed? player)
       (not (player/goalie? player))))

(defn goalie? [player]
  (and (player/dressed? player)
       (player/goalie? player)))

(defn pick-shooters [team]
  (->> (:players team)
       (vals)
       (filter shooter?)
       (map #(assoc % :shootout-skill (player/calculate-match-attack team %)))
       (sort-by :shootout-skill)
       (reverse)
       (cycle)))

(defn pick-goalie [team]
  (->> (:players team)
       (vals)
       (filter goalie?)
       (map #(assoc % :shootout-skill (player/calculate-match-defense team %)))
       (sort-by :defense)
       (last)))

(defn simulate-shootout-round [state round shooter goalie]
  (if (goal? (:shootout-skill shooter) (:shootout-skill goalie))
    (add-shootout-goal state round (:match-team shooter) (:id shooter) (:match-team goalie) (:id goalie))
    (add-shootout-miss state round (:match-team shooter) (:id shooter) (:match-team goalie) (:id goalie))))

(defn simulate-extra-rounds [state shooters-a shooters-b goalie-a goalie-b]
  (loop [state* state
         shooters-a* shooters-a
         shooters-b* shooters-b
         round 3]
    (if (shootouts-tied? state*)
      (recur (-> state*
                 (simulate-shootout-round round (first shooters-a*) goalie-b)
                 (simulate-shootout-round round (first shooters-b*) goalie-a))
             (if (empty? shooters-a*) shooters-a (rest shooters-a*))
             (rest shooters-b*)
             (inc round))
      state*)))

(defn simulate-first-round [state shooters-a shooters-b goalie-a goalie-b]
  (loop [state* state
         shooters-a* shooters-a
         shooters-b* shooters-b
         round 0]
    (if (< round 3)
      (recur (-> state*
                 (simulate-shootout-round round (first shooters-a*) goalie-b)
                 (simulate-shootout-round round (first shooters-b*) goalie-a))
             (rest shooters-a*)
             (rest shooters-b*)
             (inc round))
      state*)))

(defn simulate-shootouts* [state shooters-a shooters-b goalie-a goalie-b]
  (let [state* (-> state (assoc-in [:teams :home :shootouts] 0)
                         (assoc-in [:teams :away :shootouts] 0)
                         (simulate-first-round shooters-a shooters-b goalie-a goalie-b))]
    (if (shootouts-tied? state*)
      (simulate-extra-rounds state* shooters-a shooters-b goalie-a goalie-b)
      state*)))

(defn simulate-shootouts [state]
  (let [shooters-a (pick-shooters (get-in state [:teams :home]))
        shooters-b (pick-shooters (get-in state [:teams :away]))
        goalie-a (pick-goalie (get-in state [:teams :home]))
        goalie-b (pick-goalie (get-in state [:teams :away]))
        state* (simulate-shootouts* state shooters-a shooters-b goalie-a goalie-b)]
    (if (> (get-in state* [:teams :home :shootouts])
           (get-in state* [:teams :away :shootouts]))
      (update-in state* [:teams :home :goals] inc)
      (update-in state* [:teams :away :goals] inc))))

