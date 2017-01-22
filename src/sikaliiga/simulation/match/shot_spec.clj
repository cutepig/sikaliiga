(ns sikaliiga.simulation.match.shot-spec
  (:require [speclj.core :refer :all]
            [clojure.spec :as s]
            [sikaliiga.simulation.match.shot :as shot]))

(s/check-asserts true)

(describe
  "shot?"
  (it "returns true when rand is less than attack"
      (should (shot/shot? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than attack"
      (should-not (shot/shot? 1.0 1.0 (constantly 1.0)))))

(describe
  "blocked?"
  (it "returns true when rand is less than defense"
      (should (shot/blocked? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than defense"
      (should-not (shot/blocked? 1.0 1.0 (constantly 1.0)))))

(describe
  "missed?"
  (it "returns true when rand is less than defense"
      (should (shot/missed? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than defense"
      (should-not (shot/missed? 1.0 1.0 (constantly 1.0)))))

(describe
  "goal?"
  (it "returns true when rand is less than attack"
      (should (shot/goal? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than attack"
      (should-not (shot/goal? 1.0 1.0 (constantly 1.0)))))

