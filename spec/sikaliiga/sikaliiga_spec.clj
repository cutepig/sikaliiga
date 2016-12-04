(ns sikaliiga.sikaliiga-spec
  (:require [speclj.core :refer :all]
            [sikaliiga.sikaliiga :as sikaliiga]))

(describe "shot?"
          (it "returns true when dice is less than attack"
              (should (sikaliiga/shot? 1.0 1.0 (constantly 0.0))))
          (it "returns false dice is more than attack"
              (should-not (sikaliiga/shot? 1.0 1.0 (constantly 2.0)))))
