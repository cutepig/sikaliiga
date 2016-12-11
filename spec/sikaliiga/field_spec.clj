(ns sikaliiga.field-spec
  (:require [speclj.core :refer :all]
            [sikaliiga.player :as player]
            [sikaliiga.field :as field]))

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

  (defn make-test-player [id attack defense position]
    {:id id :position position :attack attack :defense defense :fitness 1 :morale 1})

  (it "returns best when candidate is excluded"
      (let [best (make-test-player nil 1.0 1.0 ::player/center)
            candidate (make-test-player #uuid "b64b733f-6cdf-496b-a37e-69f7f61affc0" 1.0 1.0 ::player/center)]
        (should= [best nil] (field/compare-substitute [best nil] candidate ::player/center {} [(:id candidate)]))))

  (describe
    "when candidate is not excluded"
    (it "returns candidate when best is nil"
      (let [candidate (make-test-player nil 1.0 1.0 ::player/center)]
        (should= [candidate 1.0] (field/compare-substitute [nil nil] candidate ::player/center {} []))))

    (describe
      "when requested position is center"
      (let [best (make-test-player nil 0.5 0.5 ::player/center)]
        (it "favors overall skill over attack or defense"
            (let [candidate (make-test-player nil 0.75 0.0 ::player/center)]
              (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/center {} [])))
            (let [candidate (make-test-player nil 0.0 0.75 ::player/center)]
              (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/center {} []))))))

    (describe
      "when requested position is a winger"
      (let [best (make-test-player nil 0.5 0 ::player/left-wing)]
        (describe
          "favors attack over defense or overall skill"
          (it "when candidate is a forward with higher defense but lesser overall skill"
              (let [candidate (make-test-player nil 0.25 1 ::player/left-wing)]
                (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/left-wing {} []))))
          (it "when candidate is a defender with higher attack"
              (let [candidate (make-test-player nil 0.75 0 ::player/defense)]
                (should= [candidate 0.75] (field/compare-substitute [best 0.5] candidate ::player/left-wing {} [])))))))

    (describe
      "when requested position is defender"
      (let [best (make-test-player nil 0.5 0.5 ::player/defense)]
        (describe
          "favors defense over attack or overall skill"
          (it "when candidate is a defender with higher attack but lesser overall skill"
              (let [candidate (make-test-player nil 0.75 0 ::player/defense)]
                (should= [best 0.5] (field/compare-substitute [best 0.5] candidate ::player/defense {} []))))
          (it "when candidate is a forward with higher defense"
              (let [candidate (make-test-player nil 0 0.75 ::player/center)]
                (should= [candidate 0.75] (field/compare-substitute [best 0.5] candidate ::player/defense {} [])))))))))
