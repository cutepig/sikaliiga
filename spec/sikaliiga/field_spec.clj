(ns sikaliiga.field-spec
  (:require [speclj.core :refer :all]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]
            [sikaliiga.team :as team]
            [sikaliiga.field :as field]))

(def team (team/make-test-team 50 75))

(describe
  "get-player-by-index"
  (let [field [:goalie [:defender-1 :defender-2] [:left-wing :center :right-wing]]]
    (it "returns the correct goalie"
        (should= :goalie (field/get-player-by-index field 0)))

    (it "returns the correct first defender"
        (should= :defender-1 (field/get-player-by-index field 1)))

    (it "returns the correct second defender"
        (should= :defender-2 (field/get-player-by-index field 2)))

    (it "returns the correct left-wing"
        (should= :left-wing (field/get-player-by-index field 3)))

    (it "returns the correct center"
        (should= :center (field/get-player-by-index field 4)))

    (it "returns the correct right-wing"
        (should= :right-wing (field/get-player-by-index field 5)))

    ;; TODO:
    (it "returns the correct extra attacker"
        (pending "TODO"))))

(describe
  "compare-substitute"

  (it "returns candidate when best is nil"
    (let [candidate (player/make-test-player nil 1.0 1.0 ::player/center nil)]
      (should= [candidate 1.0] (field/compare-substitute [nil nil] candidate ::player/center {}))))

  (describe
    "when requested position is center"
    (let [best (player/make-test-player nil 0.5 0.5 ::player/center nil)]
      (it "favors overall skill over attack or defense"
          (let [candidate (player/make-test-player nil 0.75 0.0 ::player/center nil)]
            (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/center {})))
          (let [candidate (player/make-test-player nil 0.0 0.75 ::player/center nil)]
            (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/center {}))))))

    (describe
      "when requested position is a winger"
      (let [best (player/make-test-player nil 0.5 0 ::player/left-wing nil)]
        (describe
          "favors attack over defense or overall skill"
          (it "when candidate is a forward with higher defense but lesser overall skill"
              (let [candidate (player/make-test-player nil 0.25 1 ::player/left-wing nil)]
                (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/left-wing {}))))
          (it "when candidate is a defender with higher attack"
              (let [candidate (player/make-test-player nil 0.75 0 ::player/defense nil)]
                (should= [candidate 0.75] (field/compare-substitute [best 0.5] candidate ::player/left-wing {})))))))

    (describe
      "when requested position is defender"
      (let [best (player/make-test-player nil 0.5 0.5 ::player/defense nil)]
        (describe
          "favors defense over attack or overall skill"
          (it "when candidate is a defender with higher attack but lesser overall skill"
              (let [candidate (player/make-test-player nil 0.75 0 ::player/defense nil)]
                (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/defense {}))))
          (it "when candidate is a forward with higher defense"
              (let [candidate (player/make-test-player nil 0 0.75 ::player/center nil)]
                (should= [candidate 0.75] (field/compare-substitute [best 0.5] candidate ::player/defense {}))))))))

(describe
  "collect-substitute-candidates"
  (it "eliminates those in excluded list"
      (let [candidates [(player/make-test-player (util/make-uuid) 1.0 1.0 nil ::player/dressed)
                        (player/make-test-player (util/make-uuid) 1.0 1.0 nil ::player/dressed)]
            excluded [(:id (second candidates))]]
        (should= (take 1 candidates) (field/collect-substitute-candidates candidates excluded))))

  (it "eliminates non-dressed players"
      (let [candidates [(player/make-test-player nil 1 1 nil ::player/dressed)
                        (player/make-test-player nil 1 1 nil ::player/injured)]]
        (should= (take 1 candidates) (field/collect-substitute-candidates candidates [])))))

(describe
  "pick-player-for-position"
  (it "returns the original player when that player is dressed"
      (let [original (player/make-test-player (util/make-uuid) 1 1 ::player/center ::player/dressed)
            team* (assoc team :players {(:id original) original})
            field [(:id original)]]
        (should= (:id original) (field/pick-player-for-position (:id original) field team* ::player/left-wing))))

  (it "doesn't return the original player when that player is not dressed"
      (let [original (player/make-test-player (util/make-uuid) 1 1 ::player/center ::player/bench)
            team (assoc team :players {(:id original) original})
            field [(:id original)]]
        (should-not (field/pick-player-for-position (:id original) field team ::player/left-wing))))

  (it "returns a given alternative when the original player is not dressed"
      (let [original (player/make-test-player (util/make-uuid) 1 1 ::player/center ::player/bench)
            alternative (player/make-test-player (util/make-uuid) 1 1 ::player/center ::player/dressed)
            team (assoc team :players {(:id original) original (:id alternative) alternative})
            field [(:id original)]]
        (should= (:id alternative) (field/pick-player-for-position (:id original) field team ::player/left-wing))))

  (it "returns most viable alternative when the original player is not dressed"
      (let [original (player/make-test-player (util/make-uuid) 1 1 ::player/center ::player/bench)
            best (player/make-test-player (util/make-uuid) 0.75 0.75 ::player/center ::player/dressed)
            lesser (player/make-test-player (util/make-uuid) 0.5 0.5 ::player/center ::player/dressed)
            best-injured (player/make-test-player (util/make-uuid) 1.0 1.0 ::player/center ::player/injured)
            team (assoc team :players {(:id original) original (:id best) best (:id lesser) lesser (:id best-injured) best-injured})
            field [(:id original)]]
        (should= (:id best) (field/pick-player-for-position (:id original) field team ::player/left-wing)))))

(describe
  "pick-forwards-for-field"
  (it "returns original field when all players on it are dressed"
      (let [left-wing (player/make-test-player (util/make-uuid) 1 1 ::player/left-wing ::player/dressed)
            center (player/make-test-player (util/make-uuid) 1 1 ::player/center ::player/dressed)
            right-wing (player/make-test-player (util/make-uuid) 1 1 ::player/right-wing ::player/dressed)
            team (assoc team :players {(:id left-wing) left-wing (:id center) center (:id right-wing) right-wing})
            field [(:id left-wing) (:id center) (:id right-wing)]]
        (should= field (field/pick-forwards-for-field team field))))

  (it "returns field of nils when none of the players on it are dressed"
      (let [left-wing (player/make-test-player (util/make-uuid) 1 1 ::player/left-wing ::player/injured)
            center (player/make-test-player (util/make-uuid) 1 1 ::player/center ::player/injured)
            right-wing (player/make-test-player (util/make-uuid) 1 1 ::player/right-wing ::player/injured)
            team {:players (util/key-by :id [left-wing center right-wing])}
            field [(:id left-wing) (:id center) (:id right-wing)]]
        (should= [nil nil nil] (field/pick-forwards-for-field team field))))

  (it "returns original field with extra attacker when all players on it are dressed"
      (pending "TODO"))

  (it "returns original short-handed field when all players on it are dressed"
      (pending "TODO")))

(describe
  "pick-defenders-for-field"
  (it "returns original field when all players on it are dressed"
      (let [defender-1 (player/make-test-player (util/make-uuid) 1 1 ::player/defense ::player/dressed)
            defender-2 (player/make-test-player (util/make-uuid) 1 1 ::player/defense ::player/dressed)
            team {:players (util/key-by :id [defender-1 defender-2])}
            field [(:id defender-1) (:id defender-2)]]
        (should= field (field/pick-defenders-for-field team field))))

  (it "returns field of nils when none of the players on it are dressed"
      (let [defender-1 (player/make-test-player (util/make-uuid) 1 1 ::player/defense ::player/injured)
            defender-2 (player/make-test-player (util/make-uuid) 1 1 ::player/defense ::player/injured)
            team {:players (util/key-by :id [defender-1 defender-2])}
            field [(:id defender-1) (:id defender-2)]]
        (should= [nil nil] (field/pick-defenders-for-field team field)))))
