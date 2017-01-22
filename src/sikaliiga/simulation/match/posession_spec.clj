(ns sikaliiga.simulation.match.posession-spec
  (:require [speclj.core :refer :all]
            [clojure.spec :as s]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]
            [sikaliiga.team :as team]
            [sikaliiga.simulation.match.posession :as posession]))

(s/check-asserts true)

(def team-a (team/make-test-team "Team A" 50 75))
(def team-b (team/make-test-team "Team B" 50 75))
(def state (match/prepare-state team-a team-b))

(describe
  "face-off?"
  (it "returns true in the beginning of periods"
      (should (posession/face-off? {:seconds 0}))
      (should (posession/face-off? {:seconds 1200}))
      (should (posession/face-off? {:seconds 2400}))
      (should (posession/face-off? {:seconds 3600})))

  (it "returns false when not in the beginning of periods"
      (should (not-any? #(posession/face-off? {:seconds %} (constantly 1)) (range 1 1200)))
      (should (not-any? #(posession/face-off? {:seconds %} (constantly 1)) (range 1201 2400)))
      (should (not-any? #(posession/face-off? {:seconds %} (constantly 1)) (range 2401 3600)))
      (should (not-any? #(posession/face-off? {:seconds %} (constantly 1)) (range 3601 3900))))

  (it "returns true when :last-goal is second before for either team"
      (should (posession/face-off? {:seconds 1234 :teams {:home {:last-goal 1233}}} (constantly 1)))
      (should (posession/face-off? {:seconds 1234 :teams {:away {:last-goal 1233}}} (constantly 1))))

  (it "returns true when rand is less than mean-face-offs-per-sec"
      (should (posession/face-off? {:seconds 1234} (constantly 0)))))

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
                             (assoc :events [[:face-off 0 :home (:id winner-player) :away (:id loser-player)]])
                             (assoc-in [:teams :home :players (:id winner-player) :face-offs] 1)
                             (assoc-in [:teams :home :players (:id winner-player) :face-off-wins] 1)
                             (assoc-in [:teams :away :players (:id loser-player) :face-offs] 1)
                             (assoc-in [:teams :away :players (:id loser-player) :face-off-wins] 0))
                   (posession/add-face-off state* :home (:id winner-player) :away (:id loser-player)))))))
