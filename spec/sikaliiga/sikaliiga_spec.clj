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
  "shift-forwards?"
  (it "returns true in the beginning of periods"
      (should (sikaliiga/shift-forwards? {:seconds 0} {:next-shift-forwards 99999}))
      (should (sikaliiga/shift-forwards? {:seconds 1200} {:next-shift-forwards 99999}))
      (should (sikaliiga/shift-forwards? {:seconds 2400} {:next-shift-forwards 99999}))
      (should (sikaliiga/shift-forwards? {:seconds 3600} {:next-shift-forwards 99999})))

  (it "returns false when not in the beginning of periods"
      (should (not-any? #(sikaliiga/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 1 1200)))
      (should (not-any? #(sikaliiga/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 1201 2400)))
      (should (not-any? #(sikaliiga/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 2401 3600)))
      (should (not-any? #(sikaliiga/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 3601 3900))))

  (it "returns true when next-shift-forwards is reached"
      (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 10})))

  (it "returns true when next-shift-forwards is nil"
      (should (sikaliiga/shift-forwards? {:seconds 9} {})))

  (it "returns false when next-shift-forwards is not reached"
      (should-not (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20})))

  (it "returns true when not on power-play and current field is power-play"
      (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 4}))
      (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 5})))

  (it "returns true when not short-handed and current field is short-handed"
      (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 6}))
      (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 7})))

  (describe
    "when on power-play"
    (it "returns true when current field is not power-play"
        (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 0 :power-play? true}))))

  (describe
    "when short-handed"
    (it "returns true when current field is not short-handed"
        (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 0 :short-handed? true})))))

(describe
  "shift-defenders?"
  (it "returns true in the beginning of periods"
      (should (sikaliiga/shift-defenders? {:seconds 0} {:next-shift-defenders 99999}))
      (should (sikaliiga/shift-defenders? {:seconds 1200} {:next-shift-defenders 99999}))
      (should (sikaliiga/shift-defenders? {:seconds 2400} {:next-shift-defenders 99999}))
      (should (sikaliiga/shift-defenders? {:seconds 3600} {:next-shift-defenders 99999})))

  (it "returns false when not in the beginning of periods"
      (should (not-any? #(sikaliiga/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 1 1200)))
      (should (not-any? #(sikaliiga/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 1201 2400)))
      (should (not-any? #(sikaliiga/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 2401 3600)))
      (should (not-any? #(sikaliiga/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 3601 3900))))

  (it "returns true when next-shift-defenders is reached"
      (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 10})))

  (it "returns true when next-shift-defenders is nil"
      (should (sikaliiga/shift-defenders? {:seconds 9} {})))

  (it "returns false when next-shift-defenders is not reached"
      (should-not (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20})))

  (it "returns true when not on power-play and current field is power-play"
      (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 3}))
      (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 4})))

  (it "returns true when not short-handed and current field is short-handed"
      (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 5}))
      (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 6})))

  (describe
    "when on power-play"
    (it "returns true when current field is not power-play"
        (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 0 :power-play? true}))))

  (describe
    "when short-handed"
    (it "returns true when current field is not short-handed"
        (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 0 :short-handed? true})))))

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
  (it "TODO"
      (pending "TODO")))

