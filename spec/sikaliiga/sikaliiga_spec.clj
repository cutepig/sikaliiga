(ns sikaliiga.sikaliiga-spec
  (:require [speclj.core :refer :all]
            [clojure.spec :as s]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]
            [sikaliiga.team :as team]
            [sikaliiga.sikaliiga :as sikaliiga]))

(s/check-asserts true)

(def team-a (team/make-test-team 50 75))
(def team-b (team/make-test-team 50 75))
(def state (sikaliiga/prepare-state team-a team-b))

(describe
  "shot?"
  (it "returns true when rand is less than attack"
      (should (sikaliiga/shot? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than attack"
      (should-not (sikaliiga/shot? 1.0 1.0 (constantly 1.0)))))

(describe
  "blocked?"
  (it "returns true when rand is less than defense"
      (should (sikaliiga/blocked? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than defense"
      (should-not (sikaliiga/blocked? 1.0 1.0 (constantly 1.0)))))

(describe
  "missed?"
  (it "returns true when rand is less than defense"
      (should (sikaliiga/missed? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than defense"
      (should-not (sikaliiga/missed? 1.0 1.0 (constantly 1.0)))))

(describe
  "goal?"
  (it "returns true when rand is less than attack"
      (should (sikaliiga/goal? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than attack"
      (should-not (sikaliiga/goal? 1.0 1.0 (constantly 1.0)))))

(describe
  "face-off?"
  (it "returns true in the beginning of periods"
      (should (sikaliiga/face-off? {:seconds 0}))
      (should (sikaliiga/face-off? {:seconds 1200}))
      (should (sikaliiga/face-off? {:seconds 2400}))
      (should (sikaliiga/face-off? {:seconds 3600})))

  (it "returns false when not in the beginning of periods"
      (should (not-any? #(sikaliiga/face-off? {:seconds %}) (range 1 1200)))
      (should (not-any? #(sikaliiga/face-off? {:seconds %}) (range 1201 2400)))
      (should (not-any? #(sikaliiga/face-off? {:seconds %}) (range 2401 3600)))
      (should (not-any? #(sikaliiga/face-off? {:seconds %}) (range 3601 3900))))

  (it "returns true when :face-off? is set in state"
      (should (sikaliiga/face-off? {:face-off? true})))

  (it "returns false when :face-off? is not set in state"
      (should-not (sikaliiga/face-off? {:face-off? false}))))

(describe
  "add-face-off"
  (describe
    "when given pristine state"
    (let [winner-player (player/make-test-player (util/make-uuid) 1 1 ::player/center ::player/dressed)
          loser-player (player/make-test-player (util/make-uuid) 0.75 0.75 ::player/center ::player/dressed)
          state* (-> state
                     (assoc :sentinel :foo)
                     (assoc :seconds 0)
                     (assoc-in [:teams :home :players] {(:id winner-player) winner-player})
                     (assoc-in [:teams :away :players] {(:id loser-player) loser-player}))]
      (it "returns full modified state"
          (should= (-> state* (assoc :posession :home)
                             (assoc :events [[:face-off 0 (:id winner-player) (:id loser-player)]])
                             (assoc-in [:teams :home :players (:id winner-player) :face-offs] 1)
                             (assoc-in [:teams :home :players (:id winner-player) :face-off-wins] 1)
                             (assoc-in [:teams :away :players (:id loser-player) :face-offs] 1)
                             (assoc-in [:teams :away :players (:id loser-player) :face-off-wins] 0))
                   (sikaliiga/add-face-off state* :home (:id winner-player) :away (:id loser-player)))))))

(describe
  "shift-forwards*"
  (it "returns requested field"
      (let [state* (assoc state :seconds 0)
            expected [nil nil (:players (first (get-in team-a [:fields :forwards])))]
            actual (get-in (sikaliiga/shift-forwards* state* (get-in state [:teams :home]) 0) [:teams :home :field])]
        (should= expected actual))))

(describe
  "shift-forwards"
  (it "returns first field in state team on period starts"
      (let [state* (assoc state :seconds 0)
            expected [nil nil (:players (first (get-in team-a [:fields :forwards])))]
            actual (get-in (sikaliiga/shift-forwards state* (get-in state [:teams :home])) [:teams :home :field])]
        (should= expected actual)))

  (it "returns state as was when this team doesn't have posession"
      (let [state* (assoc state :posession :away :seconds 1)
            expected state*]
        (should= expected (sikaliiga/shift-forwards expected (get-in state [:teams :home])))))

  (it "returns the next field when shift time is up"
      (let [lines (range 4)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-forwards 1 :current-field-forwards %))
            expected (map #(vector nil nil (:players (nth (get-in team-a [:fields :forwards]) (util/mod-to-range (inc %) 0 3)))) lines)
            actual (map #(get-in (sikaliiga/shift-forwards % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns the next power-play field when shift time is up"
      (let [lines (range 4 6)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-forwards 1 :power-play? true :current-field-forwards %))
            expected (map #(vector nil nil (:players (nth (get-in team-a [:fields :forwards]) (util/mod-to-range (inc %) 4 5)))) lines)
            actual (map #(get-in (sikaliiga/shift-forwards % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns the next short-handed field when shift time is up"
      (let [lines (range 6 8)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-forwards 1 :power-play? true :current-field-forwards %))
            expected (map #(vector nil nil (:players (nth (get-in team-a [:fields :forwards]) (util/mod-to-range (inc %) 6 7)))) lines)
            actual (map #(get-in (sikaliiga/shift-forwards % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns first power-play field on power-play when current field is not power-play"
      (let [state* (-> state (assoc :seconds 1 :posession :home)
                             (assoc-in [:teams :home :power-play?] true)
                             (assoc-in [:teams :home :next-shift-forwards] 999)
                             (assoc-in [:teams :home :current-field-forwards] 0))
            expected [nil nil (:players (nth (get-in team-a [:fields :forwards]) 4))]
            actual (get-in (sikaliiga/shift-forwards state* (get-in state* [:teams :home])) [:teams :home :field])]
        (should= expected actual)))

  (it "returns first short-handed field on short-handed when current field is not short-handed"
      (let [state* (-> state (assoc :seconds 1 :posession :home)
                             (assoc-in [:teams :home :short-handed?] true)
                             (assoc-in [:teams :home :next-shift-forwards] 999)
                             (assoc-in [:teams :home :current-field-forwards] 0))
            expected [nil nil (:players (nth (get-in team-a [:fields :forwards]) 6))]
            actual (get-in (sikaliiga/shift-forwards state* (get-in state* [:teams :home])) [:teams :home :field])]
        (should= expected actual))))

(describe
  "shift-defenders"
  (it "returns first field in state team on period starts"
      (let [state* (assoc state :seconds 0)
            expected [nil (:players (first (get-in team-a [:fields :defenders]))) nil]
            actual (get-in (sikaliiga/shift-defenders state* (get-in state [:teams :home])) [:teams :home :field])]
        (should= expected actual)))

  (it "returns state as was when this team doesn't have posession"
      (let [state* (assoc state :posession :away :seconds 1)
            expected state*]
        (should= expected (sikaliiga/shift-defenders expected (get-in state [:teams :home])))))

  (it "returns state as was when this team doesn't have posession"
      (let [state* (assoc state :posession :away :seconds 1)
            expected state*]
        (should= expected (sikaliiga/shift-defenders expected (get-in state [:teams :home])))))

  (it "returns the next field when shift time is up"
      (let [lines (range 3)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-defenders 1 :current-field-defenders %))
            expected (map #(vector nil (:players (nth (get-in team-a [:fields :defenders]) (util/mod-to-range (inc %) 0 2))) nil) lines)
            actual (map #(get-in (sikaliiga/shift-defenders % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns the next power-play field when shift time is up"
      (let [lines (range 3 5)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-defenders 1 :power-play? true :current-field-defenders %))
            expected (map #(vector nil (:players (nth (get-in team-a [:fields :defenders]) (util/mod-to-range (inc %) 3 4))) nil) lines)
            actual (map #(get-in (sikaliiga/shift-defenders % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns the next short-handed field when shift time is up"
      (let [lines (range 5 7)
            state* (map lines #(assoc state :seconds 1 :posession :home :next-shift-defenders 1 :power-play? true :current-field-defenders %))
            expected (map #(vector nil (:players (nth (get-in team-a [:fields :defenders]) (util/mod-to-range (inc %) 5 6))) nil) lines)
            actual (map #(get-in (sikaliiga/shift-defenders % (get-in state [:teams :home])) [:teams :home :field]) state*)]
        (map #(should= %1 %2) expected actual)))

  (it "returns first power-play field on power-play when current field is not power-play"
      (let [state* (-> state (assoc :seconds 1 :posession :home)
                             (assoc-in [:teams :home :power-play?] true)
                             (assoc-in [:teams :home :next-shift-defenders] 999)
                             (assoc-in [:teams :home :current-field-defenders] 0))
            expected [nil (:players (nth (get-in team-a [:fields :defenders]) 3)) nil]
            actual (get-in (sikaliiga/shift-defenders state* (get-in state* [:teams :home])) [:teams :home :field])]
        (should= expected actual)))

  (it "returns first short-handed field on short-handed when current field is not short-handed"
      (let [state* (-> state (assoc :seconds 1 :posession :home)
                             (assoc-in [:teams :home :short-handed?] true)
                             (assoc-in [:teams :home :next-shift-defenders] 999)
                             (assoc-in [:teams :home :current-field-defenders] 0))
            expected [nil (:players (nth (get-in team-a [:fields :defenders]) 5)) nil]
            actual (get-in (sikaliiga/shift-defenders state* (get-in state* [:teams :home])) [:teams :home :field])]
        (should= expected actual))))

(describe
  "shift-goalie"
  (it "returns first goalie when goalie is nil"
      (let [state* (assoc state :seconds 0)
            expected (first (get-in state* [:teams :home :fields :goalies]))
            actual (get-in (sikaliiga/shift-goalie state* (get-in state [:teams :home])) [:teams :home :field 0])]
        (should= expected actual)))

  (it "returns state unchanged when goalie is set"
      (let [state* (-> state (assoc :seconds 0)
                             (assoc-in [:teams :home :field 0] (first (get-in team-a [:fields :goalies]))))
            expected state*
            actual (sikaliiga/shift-goalie state* (get-in state [:teams :home]))]
        (should= expected actual))))

(describe
  "simulate-penalty-release*"
  (it "returns penalty box unchanged when no penalties are set to release"
      (let [player (first (get-in state [:teams :home :players]))
            state* (-> state (assoc :seconds 0)
                             (assoc-in [:teams :home :penalty-box]
                                       {(:id player) {:time 0 :length 120}}))
            expected (get-in state* [:teams :home :penalty-box])
            actual (get-in (sikaliiga/simulate-penalty-release* state* (get-in state* [:teams :home]))
                           [:teams :home :penalty-box])]
        (should= expected actual)))

  (it "returns empty penalty box when all penalties are set to release"
      (let [player (first (get-in state [:teams :home :players]))
            state* (-> state (assoc :seconds 120)
                             (assoc-in [:teams :home :penalty-box]
                                       {(:id player) {:time 0 :length 120}}))
            expected {}
            actual (get-in (sikaliiga/simulate-penalty-release* state* (get-in state* [:teams :home]))
                           [:teams :home :penalty-box])]
        (should= expected actual))))
