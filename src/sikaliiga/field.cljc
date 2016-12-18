(ns sikaliiga.field
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]))

(s/check-asserts true)

;; Field can be [nil] [[#uuid]] or [[#uuid nil]]
;; So players can be nil [#uuid] or [#uuid nil]
(s/def ::players (s/or :players/nil nil? :players/coll (s/coll-of (s/or :player/nil nil? :player/player uuid?))))
(s/def ::field (s/or :field/nil nil? :field/players (s/coll-of ::players)))

(defn get-players [field]
  (flatten (rest field)))

(defn get-goalie [field]
  (first field))

(defn get-defenders [field]
  (second field))

(defn get-defender [field n]
  (nth (get-defenders field) n))

(defn get-forwards [field]
  (nth field 2))

(defn get-left-wing [field]
  (first (get-forwards field)))

(defn get-center [field]
  (s/assert ::field field)
  (let [forwards (get-forwards field)
        center (second forwards)]
    center))

(defn get-right-wing [field]
  (nth (get-forwards field) 2))

(defn get-extra-forward [field]
  (nth (get-forwards field) 3))

(defn get-player-by-index [field index]
  (s/assert (s/cat :field ::field :index ::index) [field index])
  (cond
    (> index 2) (nth (nth field 2) (- index 3))
    (> index 0) (nth (second field) (dec index))
    :else (first field)))

(defn get-position-by-index [index]
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
  (s/assert (s/cat :pick-player-for-position/id uuid?
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

(defn pick-forwards-for-field [team field]
  (s/assert (s/cat :pick-forwards-for-field/team map?
                   :pick-forwards-for-field/field ::players)
    [team field])
  ;; FIXME: This eagerly picks a substitute for left-wing that might be better suitable as center
  ;; if the original center is not dressed and requires a substitute too
  (reduce #(pick-forward-for-position team %1 %2)
          field
          (range (count field))))

(defn pick-defenders-for-field [team field]
  (s/assert (s/cat :pick-forwards-for-field/team map?
                   :pick-forwards-for-field/field ::players)
    [team field])
  (reduce #(assoc %1 %2 (pick-player-for-position (nth %1 %2) %1 team (get-position-by-index (inc %2))))
    field
    (range (count field))))
