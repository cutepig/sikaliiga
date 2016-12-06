(ns sikaliiga.field-spec
  (:require [speclj.core :refer :all]
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
