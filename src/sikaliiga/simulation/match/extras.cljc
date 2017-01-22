(ns sikaliiga.simulation.match.extras
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.simulation.match.specs :as specs]))

(defn simulate-extras [state]
  (s/assert ::specs/state state)
  ;; TODO
  state)
