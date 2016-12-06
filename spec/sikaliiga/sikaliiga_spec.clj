(ns sikaliiga.sikaliiga-spec
  (:require [speclj.core :refer :all]
            [sikaliiga.sikaliiga :as sikaliiga]))

(describe
  "shot?"
  (it "returns true when rand is less than attack"
      (should (sikaliiga/shot? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than attack"
      (should-not (sikaliiga/shot? 1.0 1.0 (constantly 2.0)))))

(describe
  "blocked?"
  (it "returns true when rand is less than defense"
      (should (sikaliiga/blocked? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than defense"
      (should-not (sikaliiga/blocked? 1.0 1.0 (constantly 2.0)))))

(describe
  "missed?"
  (it "returns true when rand is less than defense"
      (should (sikaliiga/missed? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than defense"
      (should-not (sikaliiga/missed? 1.0 1.0 (constantly 2.0)))))

(describe
  "goal?"
  (it "returns true when rand is less than attack"
      (should (sikaliiga/goal? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than attack"
      (should-not (sikaliiga/goal? 1.0 1.0 (constantly 2.0)))))

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
