(ns sikaliiga.simulation.match.penalties-spec
  (:require [speclj.core :refer :all]
            [clojure.spec :as s]
            [sikaliiga.util :as util]
            [sikaliiga.team :as team]
            [sikaliiga.simulation.match.penalties :as penalties]))

(s/check-asserts true)

(def team-a (team/make-test-team "Team A" 50 75))
(def team-b (team/make-test-team "Team B" 50 75))
(def state (match/prepare-state team-a team-b))

(describe
  "simulate-penalty-release*"
  (it "returns penalty box unchanged when no penalties are set to release"
      (let [player (first (get-in state [:teams :home :players]))
            state* (-> state (assoc :seconds 0)
                             (assoc-in [:teams :home :penalty-box]
                                       {(:id player) {:time 0 :length 120}}))
            expected (get-in state* [:teams :home :penalty-box])
            actual (get-in (penalties/simulate-penalty-release* state* (get-in state* [:teams :home]))
                           [:teams :home :penalty-box])]
        (should= expected actual)))

  (it "returns empty penalty box when all penalties are set to release"
      (let [player (first (get-in state [:teams :home :players]))
            state* (-> state (assoc :seconds 120)
                             (assoc-in [:teams :home :penalty-box]
                                       {(:id player) {:time 0 :length 120}}))
            expected {}
            actual (get-in (penalties/simulate-penalty-release* state* (get-in state* [:teams :home]))
                           [:teams :home :penalty-box])]
        (should= expected actual))))
