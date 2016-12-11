(ns sikaliiga.field-spec
  (:require [speclj.core :refer :all]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]
            [sikaliiga.field :as field]))

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
  "shift-forwards?"
  (it "returns true in the beginning of periods"
      (should (field/shift-forwards? {:seconds 0} {:next-shift-forwards 99999}))
      (should (field/shift-forwards? {:seconds 1200} {:next-shift-forwards 99999}))
      (should (field/shift-forwards? {:seconds 2400} {:next-shift-forwards 99999}))
      (should (field/shift-forwards? {:seconds 3600} {:next-shift-forwards 99999})))

  (it "returns false when not in the beginning of periods"
      (should (not-any? #(field/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 1 1200)))
      (should (not-any? #(field/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 1201 2400)))
      (should (not-any? #(field/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 2401 3600)))
      (should (not-any? #(field/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 3601 3900))))

  (it "returns true when next-shift-forwards is reached"
      (should (field/shift-forwards? {:seconds 10} {:next-shift-forwards 10})))

  (it "returns true when next-shift-forwards is nil"
      (should (field/shift-forwards? {:seconds 9} {})))

  (it "returns false when next-shift-forwards is not reached"
      (should-not (field/shift-forwards? {:seconds 10} {:next-shift-forwards 20})))

  (it "returns true when not on power-play and current field is power-play"
      (should (field/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 4}))
      (should (field/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 5})))

  (it "returns true when not short-handed and current field is short-handed"
      (should (field/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 6}))
      (should (field/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 7})))

  (describe
    "when on power-play"
    (it "returns true when current field is not power-play"
        (should (field/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 0 :power-play? true}))))

  (describe
    "when short-handed"
    (it "returns true when current field is not short-handed"
        (should (field/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 0 :short-handed? true})))))

(describe
  "shift-defenders?"
  (it "returns true in the beginning of periods"
      (should (field/shift-defenders? {:seconds 0} {:next-shift-defenders 99999}))
      (should (field/shift-defenders? {:seconds 1200} {:next-shift-defenders 99999}))
      (should (field/shift-defenders? {:seconds 2400} {:next-shift-defenders 99999}))
      (should (field/shift-defenders? {:seconds 3600} {:next-shift-defenders 99999})))

  (it "returns false when not in the beginning of periods"
      (should (not-any? #(field/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 1 1200)))
      (should (not-any? #(field/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 1201 2400)))
      (should (not-any? #(field/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 2401 3600)))
      (should (not-any? #(field/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 3601 3900))))

  (it "returns true when next-shift-defenders is reached"
      (should (field/shift-defenders? {:seconds 10} {:next-shift-defenders 10})))

  (it "returns true when next-shift-defenders is nil"
      (should (field/shift-defenders? {:seconds 9} {})))

  (it "returns false when next-shift-defenders is not reached"
      (should-not (field/shift-defenders? {:seconds 10} {:next-shift-defenders 20})))

  (it "returns true when not on power-play and current field is power-play"
      (should (field/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 3}))
      (should (field/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 4})))

  (it "returns true when not short-handed and current field is short-handed"
      (should (field/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 5}))
      (should (field/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 6})))

  (describe
    "when on power-play"
    (it "returns true when current field is not power-play"
        (should (field/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 0 :power-play? true}))))

  (describe
    "when short-handed"
    (it "returns true when current field is not short-handed"
        (should (field/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 0 :short-handed? true})))))

(describe
  "compare-substitute"

  (defn make-test-player [id attack defense position status]
    {:id id :position position :status status :attack attack :defense defense :fitness 1 :morale 1})

  (it "returns candidate when best is nil"
    (let [candidate (make-test-player nil 1.0 1.0 ::player/center nil)]
      (should= [candidate 1.0] (field/compare-substitute [nil nil] candidate ::player/center {}))))

  (describe
    "when requested position is center"
    (let [best (make-test-player nil 0.5 0.5 ::player/center nil)]
      (it "favors overall skill over attack or defense"
          (let [candidate (make-test-player nil 0.75 0.0 ::player/center nil)]
            (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/center {})))
          (let [candidate (make-test-player nil 0.0 0.75 ::player/center nil)]
            (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/center {}))))))

    (describe
      "when requested position is a winger"
      (let [best (make-test-player nil 0.5 0 ::player/left-wing nil)]
        (describe
          "favors attack over defense or overall skill"
          (it "when candidate is a forward with higher defense but lesser overall skill"
              (let [candidate (make-test-player nil 0.25 1 ::player/left-wing nil)]
                (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/left-wing {}))))
          (it "when candidate is a defender with higher attack"
              (let [candidate (make-test-player nil 0.75 0 ::player/defense nil)]
                (should= [candidate 0.75] (field/compare-substitute [best 0.5] candidate ::player/left-wing {})))))))

    (describe
      "when requested position is defender"
      (let [best (make-test-player nil 0.5 0.5 ::player/defense nil)]
        (describe
          "favors defense over attack or overall skill"
          (it "when candidate is a defender with higher attack but lesser overall skill"
              (let [candidate (make-test-player nil 0.75 0 ::player/defense nil)]
                (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/defense {}))))
          (it "when candidate is a forward with higher defense"
              (let [candidate (make-test-player nil 0 0.75 ::player/center nil)]
                (should= [candidate 0.75] (field/compare-substitute [best 0.5] candidate ::player/defense {}))))))))

(describe
  "collect-substitute-candidates"
  (it "eliminates those in excluded list"
      (let [candidates [(make-test-player #uuid "b64b733f-6cdf-496b-a37e-69f7f61affc0" 1.0 1.0 nil ::player/dressed)
                        (make-test-player #uuid "0b8dc0a0-8998-4d71-bc42-bb4e8eb942e0" 1.0 1.0 nil ::player/dressed)]
            excluded [#uuid "0b8dc0a0-8998-4d71-bc42-bb4e8eb942e0"]]
        (should= (take 1 candidates) (field/collect-substitute-candidates candidates excluded))))

  (it "eliminates non-dressed players"
      (let [candidates [(make-test-player nil 1 1 nil ::player/dressed)
                        (make-test-player nil 1 1 nil ::player/injured)]]
        (should= (take 1 candidates) (field/collect-substitute-candidates candidates [])))))

(describe
  "pick-player-for-position"
  (it "returns the original player when that player is dressed"
      (let [original {:id #uuid "ec4d818c-cb7c-45ef-944a-009c1d6709ab" :status ::player/dressed}
            team {:players (util/key-by :id [original])}
            field [(:id original)]]
        (should= (:id original) (field/pick-player-for-position (:id original) field team ::player/left-wing))))

  (it "doesn't return the original player when that player is not dressed"
      (let [original {:id #uuid "ec4d818c-cb7c-45ef-944a-009c1d6709ab" :status ::player/injured}
            team {:players (util/key-by :id [original])}
            field [(:id original) nil nil]]
        (should-not (field/pick-player-for-position (:id original) field team ::player/left-wing))))

  (it "returns a given alternative when the original player is not dressed"
      (let [original {:id #uuid "ec4d818c-cb7c-45ef-944a-009c1d6709ab" :status ::player/injured}
            alternative (make-test-player #uuid "a0acd128-61c2-479f-aede-6dc154a31a9d" 1.0 1.0 ::player/center ::player/dressed)
            team {:players (util/key-by :id [original alternative])}
            field [(:id original) nil nil]]
        (should= (:id alternative) (field/pick-player-for-position (:id original) field team ::player/left-wing))))

  (it "returns most viable alternative when the original player is not dressed"
      (let [original {:id #uuid "ec4d818c-cb7c-45ef-944a-009c1d6709ab" :status ::player/injured}
            best (make-test-player #uuid "a0acd128-61c2-479f-aede-6dc154a31a9d" 0.75 0.75 ::player/center ::player/dressed)
            lesser (make-test-player #uuid "a0acd128-61c2-479f-aede-6dc154a31a9d" 0.5 0.5 ::player/center ::player/dressed)
            best-injured (make-test-player #uuid "a0acd128-61c2-479f-aede-6dc154a31a9d" 1.0 1.0 ::player/center ::player/injured)
            team {:players (util/key-by :id [original best lesser best-injured])}
            field [(:id original) nil nil]]
        (should= (:id best) (field/pick-player-for-position (:id original) field team ::player/left-wing)))))

(describe
  "pick-forwards-for-field"
  (it "returns original field when all players on it are dressed"
      (let [left-wing (make-test-player #uuid "3838a7c8-67c8-4ee1-8587-a6f531cf19d5" 1 1 ::player/left-wing ::player/dressed)
            center (make-test-player #uuid "3838a7c8-67c8-4ee1-8587-a6f531cf19d5" 1 1 ::player/center ::player/dressed)
            right-wing (make-test-player #uuid "3838a7c8-67c8-4ee1-8587-a6f531cf19d5" 1 1 ::player/right-wing ::player/dressed)
            team {:players (util/key-by :id [left-wing center right-wing])}
            field [(:id left-wing) (:id center) (:id right-wing)]]
        (should= field (field/pick-forwards-for-field team field))))

  (it "returns field of nils when none of the players on it are dressed"
      (let [left-wing (make-test-player #uuid "3838a7c8-67c8-4ee1-8587-a6f531cf19d5" 1 1 ::player/left-wing ::player/injured)
            center (make-test-player #uuid "3838a7c8-67c8-4ee1-8587-a6f531cf19d5" 1 1 ::player/center ::player/injured)
            right-wing (make-test-player #uuid "3838a7c8-67c8-4ee1-8587-a6f531cf19d5" 1 1 ::player/right-wing ::player/injured)
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
      (let [defender-1 (make-test-player #uuid "3838a7c8-67c8-4ee1-8587-a6f531cf19d5" 1 1 ::player/defense ::player/dressed)
            defender-2 (make-test-player #uuid "3838a7c8-67c8-4ee1-8587-a6f531cf19d5" 1 1 ::player/defense ::player/dressed)
            team {:players (util/key-by :id [defender-1 defender-2])}
            field [(:id defender-1) (:id defender-2)]]
        (should= field (field/pick-defenders-for-field team field))))

  (it "returns field of nils when none of the players on it are dressed"
      (let [defender-1 (make-test-player #uuid "3838a7c8-67c8-4ee1-8587-a6f531cf19d5" 1 1 ::player/defense ::player/injured)
            defender-2 (make-test-player #uuid "3838a7c8-67c8-4ee1-8587-a6f531cf19d5" 1 1 ::player/defense ::player/injured)
            team {:players (util/key-by :id [defender-1 defender-2])}
            field [(:id defender-1) (:id defender-2)]]
        (should= [nil nil] (field/pick-defenders-for-field team field)))))

(describe
  "shift-forwards"
  (it "TODO"
      (pending "TODO")))

(describe
  "shift-defenders"
  (it "TODO"
      (pending "TODO")))
