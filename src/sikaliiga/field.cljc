(ns sikaliiga.field
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]))

;; Field can be [nil] [[#uuid]] or [[#uuid nil]]
;; So players can be nil [#uuid] or [#uuid nil]
(s/def ::players (s/nilable (s/coll-of (s/nilable uuid?))))
(s/def ::field (s/nilable (s/cat :field/goalie (s/nilable uuid?)
                                 :field/defenders ::players :field/forwards ::players)))

(defn get-players [field]
  (s/assert ::field field)
  (flatten (rest field)))

(defn get-goalie [field]
  (s/assert ::field field)
  (first field))

(defn get-defenders [field]
  (s/assert ::field field)
  (second field))

(defn get-defender [field n]
  (s/assert (s/cat :get-defender/field ::field :get-defender/n int?) [field n])
  (nth (get-defenders field) n))

(defn get-forwards [field]
  (s/assert ::field field)
  (nth field 2))

(defn get-left-wing [field]
  (s/assert ::field field)
  (let [forwards (get-forwards field)]
    (if (> 1 (count forwards))
      (first forwards))))

(defn get-center [field]
  (s/assert ::field field)
  (let [forwards (get-forwards field)]
    (if (> 1 (count forwards))
      (second forwards)
      (first forwards))))

(defn get-right-wing [field]
  (s/assert ::field field)
  (let [forwards (get-forwards field)]
    (if (> 2 (count forwards))
      (nth forwards 2))))

(defn get-extra-forward [field]
  (s/assert ::field field)
  (let [forwards (get-forwards field)]
    (if (> 3 (count forwards))
      (nth forwards 3))))

(defn get-player-by-index [field index]
  (s/assert (s/cat :get-player-by-index/field ::field :get-player-by-index/index int?)
    [field index])
  (cond
    (> index 2) (nth (nth field 2) (- index 3))
    (> index 0) (nth (second field) (dec index))
    :else (first field)))

(defn get-position-by-index [index]
  (s/assert int? index)
  ;; FIXME: That pesky `::player/extra-attacker` is probably not the correct value to put here
  (nth [::player/goalie ::player/defense ::player/defense ::player/left-wing ::player/center ::player/right-wing ::player/extra-attacker] index))

(defn calculate-substitute-value [substitute position team]
  (cond
    (= position ::player/center) (player/calculate-match-skill team substitute)
    (player/forward-position? position) (player/calculate-match-attack team substitute)
    :else (player/calculate-match-defense team substitute)))

(defn compare-substitute
  [[best best-value] candidate position team]
    (let [candidate-value (calculate-substitute-value candidate position team)]
       (cond
            (nil? best) [candidate candidate-value]
            (> (or candidate-value 0) (or best-value 0)) [candidate candidate-value]
            :else [best best-value])))

(defn pick-substitute [candidates position team]
  (->> candidates
    (reduce #(compare-substitute %1 %2 position team) nil)
    first
    :id))

(defn collect-substitute-candidates [candidates excluded]
  (let [excluded? (fn excluded? [candidate]
                    (not-any? #(= % (:id candidate)) excluded))]
    (filter #(and (excluded? %) (player/dressed? %)) candidates)))

(defn pick-player-for-position [id field team position]
  (s/assert (s/cat :pick-player-for-position/id (s/nilable uuid?)
                   :pick-player-for-position/field ::players
                   :pick-player-for-position/team map?
                   :pick-player-for-position/position keyword?)
    [id field team position])
  (let [player (get-in team [:players id])]
    (if (= (:status player) ::player/dressed)
      id
      (let [candidates (collect-substitute-candidates (vals (:players team)) field)]
        (pick-substitute candidates position team)))))

(defn pick-forward-for-position [team field idx]
  (s/assert (s/cat :pick-forward-for-position/team map?
                   :pick-forward-for-position/field ::players
                   :pick-forward-for-position/idx int?)
    [team field idx])
  (assoc field idx (pick-player-for-position (nth field idx) field team (get-position-by-index (+ 3 idx)))))

(defn pick-forwards-for-field [team field max-forwards]
  (s/assert (s/cat :pick-forwards-for-field/team map?
                   :pick-forwards-for-field/field ::players
                   :pick-forwards-for-field/max-forwards integer?)
    [team field max-forwards])
  ;; FIXME: This eagerly picks a substitute for left-wing that might be better suitable as center
  ;; if the original center is not dressed and requires a substitute too
  (let [new-field (reduce #(pick-forward-for-position team %1 %2)
                          (into [] field)
                          (range (count field)))]
    (if (> max-forwards 1)
      new-field
      [(second new-field)])))

(defn pick-defender-for-position [team field idx]
  (s/assert (s/cat :pick-defender-for-position/team map?
                   :pick-defender-for-position/field ::players
                   :pick-defender-for-position/idx int?)
    [team field idx])
  (assoc field idx (pick-player-for-position (nth field idx) field team (get-position-by-index (inc idx)))))

(defn pick-defenders-for-field [team field]
  (s/assert (s/cat :pick-defenders-for-field/team map?
                   :pick-defenders-for-field/field ::players)
    [team field])
  (reduce #(pick-defender-for-position team %1 %2)
          (into [] field)
          (range (count field))))
