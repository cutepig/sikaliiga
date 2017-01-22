(ns sikaliiga.simulation.match.shifts
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.simulation.match.specs :as specs]
            [sikaliiga.util :as util]
            [sikaliiga.field :as field]
            [sikaliiga.team :as team]
            [sikaliiga.simulation.match.util :as mutil]))

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

(defn shift-forwards* [state team idx]
  (s/assert (s/cat :shift-forwards*/state ::specs/state :shift-forwards*/team ::specs/team
                   :shift-forwards*/idx int?)
            [state team idx])
  (let [match-team (:match-team team)
        field (get-in team [:fields :forwards idx])
        max-forwards (max 1 (- 3 (count (:penalty-box team))))]
    (s/assert (s/cat :shift-forwards*/match-team keyword? :shift-fowards*/field ::team/field)
              [match-team field])
    (-> state
        ;; TODO: Reset on-ice? for players in old field and set it for the new field
        (assoc-in [:teams match-team :field 2] (field/pick-forwards-for-field team (:players field) max-forwards))
        (assoc-in [:teams match-team :current-field-forwards] idx)
        (assoc-in [:teams match-team :next-shift-forwards] (+ (:seconds state) (:shift-length field))))))

(defn shift-forwards [state team]
  (s/assert (s/cat :shift-forwards/state ::specs/state :shift-forwards/team ::specs/team)
            [state team])
  (cond
    ;; Always force shift to first field on period starts
    ;; FIXME: But do check power-play? and short-handed?
    (mutil/period-start? (:seconds state))
      (shift-forwards* state team 0)
    ;; Never do shift is the team doesn't have posession
    ;; FIXME: Do some probability based rand here
    (not= (:posession state) (:match-team team))
      state
    ;; FIXME: We'd need to be aware of different power-play constructions, 5-4, 5-3, 4-3
    (and (:power-play? team) (not (power-play-forwards? team)))
      ;; Power-play started, put on the first power-play field
      (shift-forwards* state team 4)
    (and (not (:power-play? team)) (power-play-forwards? team))
      ;; Power-play ended, put on the first field
      (shift-forwards* state team 0)
    ;; FIXME: We'd need to be aware of different power-play constructions, 5-4, 5-3, 4-3
    (and (:short-handed? team) (not (short-handed-forwards? team)))
      ;; Short-hand situation started, put on the first short-handed field
      (shift-forwards* state team 6)
    (and (not (:short-handed? team)) (short-handed-forwards? team))
      ;; Short-hand situation ended, put on the first field
      (shift-forwards* state team 0)
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
    :else state))

(defn shift-defenders* [state team idx]
  (s/assert (s/cat :shift-defenders*/state ::specs/state :shift-defenders*/team ::specs/team
                   :shift-defenders*/idx int?)
            [state team idx])
  (let [match-team (:match-team team)
        field (get-in team [:fields :defenders idx])]
    (s/assert (s/cat :shift-defenders*/match-team keyword? :shift-fowards*/field ::team/field)
              [match-team field])
    (-> state
        ;; TODO: Reset on-ice? for players in old field and set it for the new field
        (assoc-in [:teams match-team :field 1] (field/pick-defenders-for-field team (:players field)))
        (assoc-in [:teams match-team :current-field-defenders] idx)
        (assoc-in [:teams match-team :next-shift-defenders] (+ (:seconds state) (:shift-length field))))))

(defn shift-defenders [state team]
  (s/assert (s/cat :shift-defenders/state ::specs/state :shift-defenders/team ::specs/team)
            [state team])
  (cond
    ;; Always force shift to first field on period starts
    (mutil/period-start? (:seconds state))
      (shift-defenders* state team 0)
    ;; Never do shift is the team doesn't have posession
    ;; FIXME: Do some probability based rand here
    (not= (:posession state) (:match-team team))
      state
    ;; TODO: Test
    ;; See if the time for current shift is past due
    (>= (:seconds state) (or (:next-shift-defenders team) 0))
      (let [next-raw-idx (inc (:current-field-defenders team))
            ;; Choose the clamp range for next calculated field index
            ;; FIXME: We'd need to be aware of different short-handed constructions, 4-5, 3-5, 3-4
            next-idx (cond (:power-play? team) (util/mod-to-range next-raw-idx 3 4)
                           (:short-handed? team) (util/mod-to-range next-raw-idx 5 6)
                           :else (util/mod-to-range next-raw-idx 0 2))]
        (shift-defenders* state team next-idx))
    ;; FIXME: We'd need to be aware of different power-play constructions, 5-4, 5-3, 4-3
    (and (:power-play? team) (not (power-play-defenders? team)))
      ;; Power-play started, put on the first power-play field
      (shift-defenders* state team 3)
    ;; FIXME: We'd need to be aware of different power-play constructions, 5-4, 5-3, 4-3
    (and (:short-handed? team) (not (short-handed-defenders? team)))
      ;; Short-hand situation started, put on the first short-handed field
      (shift-defenders* state team 5)
    :else state))

(defn shift-goalie* [state team idx]
  ;; TODO: Be smarter, dressed? injured? etc..
  (let [goalie (nth (get-in team [:fields :goalies]) 0)]
    (update-in state [:teams (:match-team team) :field]
      #(assoc % 0 goalie))))

(defn shift-goalie [state team]
  (if (nil? (mutil/get-goalie team (:field team)))
    (shift-goalie* state team 0)
    state))

(defn prepare-field [team field]
  (assoc field :attack (mutil/calculate-field-attack team field)
               :defense (mutil/calculate-field-defense team field)
               :goalie (mutil/calculate-field-goalie team field)))

(defn simulate-shifts [state]
  (s/assert ::specs/state state)
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
