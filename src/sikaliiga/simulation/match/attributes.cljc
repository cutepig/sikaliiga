(ns sikaliiga.simulation.match.attributes)

(defn simulate-attrs* [state team]
  ;; TODO time-on-ice (toc), fitness
  state)

(defn simulate-attrs [state]
  (-> state
      (simulate-attrs* (get-in state [:teams :home]))
      (simulate-attrs* (get-in state [:teams :away]))))

