(ns sikaliiga.simulation.match.shifts-spec
  (:require [speclj.core :refer :all]
            [clojure.spec :as s]
            [sikaliiga.util :as util]
            [sikaliiga.team :as team]
            [sikaliiga.simulation.match.shifts :as shifts]))

(s/check-asserts true)

(def team-a (team/make-test-team "Team A" 50 75))
(def team-b (team/make-test-team "Team B" 50 75))
(def state (match/prepare-state team-a team-b))

(describe
  "shift-forwards*"
  (it "returns requested field"
      (let [state* (assoc state :seconds 0)
            expected [nil nil (:players (first (get-in team-a [:fields :forwards])))]
            actual (get-in (shifts/shift-forwards* state* (get-in state [:teams :home]) 0) [:teams :home :field])]
        (should= expected actual))))

(describe
  "shift-forwards"
  (it "returns first field in state team on period starts"
      (let [state* (assoc state :seconds 0)
            expected [nil nil (:players (first (get-in team-a [:fields :forwards])))]
            actual (get-in (shifts/shift-forwards state* (get-in state [:teams :home])) [:teams :home :field])]
        (should= expected actual)))

  (it "returns state as was when this team doesn't have posession"
      (let [state* (assoc state :posession :away :seconds 1)
            expected state*]
        (should= expected (shifts/shift-forwards expected (get-in state [:teams :home])))))

  (it "returns the next field when shift time is up"
      (let [lines (range 4)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-forwards 1 :current-field-forwards %))
            expected (map #(vector nil nil (:players (nth (get-in team-a [:fields :forwards]) (util/mod-to-range (inc %) 0 3)))) lines)
            actual (map #(get-in (shifts/shift-forwards % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns the next power-play field when shift time is up"
      (let [lines (range 4 6)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-forwards 1 :power-play? true :current-field-forwards %))
            expected (map #(vector nil nil (:players (nth (get-in team-a [:fields :forwards]) (util/mod-to-range (inc %) 4 5)))) lines)
            actual (map #(get-in (shifts/shift-forwards % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns the next short-handed field when shift time is up"
      (let [lines (range 6 8)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-forwards 1 :power-play? true :current-field-forwards %))
            expected (map #(vector nil nil (:players (nth (get-in team-a [:fields :forwards]) (util/mod-to-range (inc %) 6 7)))) lines)
            actual (map #(get-in (shifts/shift-forwards % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns first power-play field on power-play when current field is not power-play"
      (let [state* (-> state (assoc :seconds 1 :posession :home)
                             (assoc-in [:teams :home :power-play?] true)
                             (assoc-in [:teams :home :next-shift-forwards] 999)
                             (assoc-in [:teams :home :current-field-forwards] 0))
            expected [nil nil (:players (nth (get-in team-a [:fields :forwards]) 4))]
            actual (get-in (shifts/shift-forwards state* (get-in state* [:teams :home])) [:teams :home :field])]
        (should= expected actual)))

  (it "returns first short-handed field on short-handed when current field is not short-handed"
      (let [state* (-> state (assoc :seconds 1 :posession :home)
                             (assoc-in [:teams :home :short-handed?] true)
                             (assoc-in [:teams :home :next-shift-forwards] 999)
                             (assoc-in [:teams :home :current-field-forwards] 0))
            expected [nil nil (:players (nth (get-in team-a [:fields :forwards]) 6))]
            actual (get-in (shifts/shift-forwards state* (get-in state* [:teams :home])) [:teams :home :field])]
        (should= expected actual))))

(describe
  "shift-defenders"
  (it "returns first field in state team on period starts"
      (let [state* (assoc state :seconds 0)
            expected [nil (:players (first (get-in team-a [:fields :defenders]))) nil]
            actual (get-in (shifts/shift-defenders state* (get-in state [:teams :home])) [:teams :home :field])]
        (should= expected actual)))

  (it "returns state as was when this team doesn't have posession"
      (let [state* (assoc state :posession :away :seconds 1)
            expected state*]
        (should= expected (shifts/shift-defenders expected (get-in state [:teams :home])))))

  (it "returns state as was when this team doesn't have posession"
      (let [state* (assoc state :posession :away :seconds 1)
            expected state*]
        (should= expected (shifts/shift-defenders expected (get-in state [:teams :home])))))

  (it "returns the next field when shift time is up"
      (let [lines (range 3)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-defenders 1 :current-field-defenders %))
            expected (map #(vector nil (:players (nth (get-in team-a [:fields :defenders]) (util/mod-to-range (inc %) 0 2))) nil) lines)
            actual (map #(get-in (shifts/shift-defenders % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns the next power-play field when shift time is up"
      (let [lines (range 3 5)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-defenders 1 :power-play? true :current-field-defenders %))
            expected (map #(vector nil (:players (nth (get-in team-a [:fields :defenders]) (util/mod-to-range (inc %) 3 4))) nil) lines)
            actual (map #(get-in (shifts/shift-defenders % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns the next short-handed field when shift time is up"
      (let [lines (range 5 7)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-defenders 1 :power-play? true :current-field-defenders %))
            expected (map #(vector nil (:players (nth (get-in team-a [:fields :defenders]) (util/mod-to-range (inc %) 5 6))) nil) lines)
            actual (map #(get-in (shifts/shift-defenders % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns first power-play field on power-play when current field is not power-play"
      (let [state* (-> state (assoc :seconds 1 :posession :home)
                             (assoc-in [:teams :home :power-play?] true)
                             (assoc-in [:teams :home :next-shift-defenders] 999)
                             (assoc-in [:teams :home :current-field-defenders] 0))
            expected [nil (:players (nth (get-in team-a [:fields :defenders]) 3)) nil]
            actual (get-in (shifts/shift-defenders state* (get-in state* [:teams :home])) [:teams :home :field])]
        (should= expected actual)))

  (it "returns first short-handed field on short-handed when current field is not short-handed"
      (let [state* (-> state (assoc :seconds 1 :posession :home)
                             (assoc-in [:teams :home :short-handed?] true)
                             (assoc-in [:teams :home :next-shift-defenders] 999)
                             (assoc-in [:teams :home :current-field-defenders] 0))
            expected [nil (:players (nth (get-in team-a [:fields :defenders]) 5)) nil]
            actual (get-in (shifts/shift-defenders state* (get-in state* [:teams :home])) [:teams :home :field])]
        (should= expected actual))))

(describe
  "shift-goalie"
  (it "returns first goalie when goalie is nil"
      (let [state* (assoc state :seconds 0)
            expected (first (get-in state* [:teams :home :fields :goalies]))
            actual (get-in (shifts/shift-goalie state* (get-in state [:teams :home])) [:teams :home :field 0])]
        (should= expected actual)))

  (it "returns state unchanged when goalie is set"
      (let [state* (-> state (assoc :seconds 0)
                             (assoc-in [:teams :home :field 0] (first (get-in team-a [:fields :goalies]))))
            expected state*
            actual (shifts/shift-goalie state* (get-in state [:teams :home]))]
        (should= expected actual))))

(describe
  "simulate-penalty-release*"
  (it "returns penalty box unchanged when no penalties are set to release"
      (let [player (first (get-in state [:teams :home :players]))
            state* (-> state (assoc :seconds 0)
                             (assoc-in [:teams :home :penalty-box]
                                       {(:id player) {:time 0 :length 120}}))
            expected (get-in state* [:teams :home :penalty-box])
            actual (get-in (shifts/simulate-penalty-release* state* (get-in state* [:teams :home]))
                           [:teams :home :penalty-box])]
        (should= expected actual)))

  (it "returns empty penalty box when all penalties are set to release"
      (let [player (first (get-in state [:teams :home :players]))
            state* (-> state (assoc :seconds 120)
                             (assoc-in [:teams :home :penalty-box]
                                       {(:id player) {:time 0 :length 120}}))
            expected {}
            actual (get-in (shifts/simulate-penalty-release* state* (get-in state* [:teams :home]))
                           [:teams :home :penalty-box])]
        (should= expected actual))))
