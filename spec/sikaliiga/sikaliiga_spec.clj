(ns sikaliiga.sikaliiga-spec
  (:require [speclj.core :refer :all]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]
            [sikaliiga.sikaliiga :as sikaliiga]))

(describe
  "shot?"
  (it "returns true when rand is less than attack"
      (should (sikaliiga/shot? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than attack"
      (should-not (sikaliiga/shot? 1.0 1.0 (constantly 1.0)))))

(describe
  "blocked?"
  (it "returns true when rand is less than defense"
      (should (sikaliiga/blocked? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than defense"
      (should-not (sikaliiga/blocked? 1.0 1.0 (constantly 1.0)))))

(describe
  "missed?"
  (it "returns true when rand is less than defense"
      (should (sikaliiga/missed? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than defense"
      (should-not (sikaliiga/missed? 1.0 1.0 (constantly 1.0)))))

(describe
  "goal?"
  (it "returns true when rand is less than attack"
      (should (sikaliiga/goal? 1.0 1.0 (constantly 0.0))))
  (it "returns false when rand is more than attack"
      (should-not (sikaliiga/goal? 1.0 1.0 (constantly 1.0)))))

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

(describe
  "shift-forwards?"
  (it "returns true in the beginning of periods"
      (should (sikaliiga/shift-forwards? {:seconds 0} {:next-shift-forwards 99999}))
      (should (sikaliiga/shift-forwards? {:seconds 1200} {:next-shift-forwards 99999}))
      (should (sikaliiga/shift-forwards? {:seconds 2400} {:next-shift-forwards 99999}))
      (should (sikaliiga/shift-forwards? {:seconds 3600} {:next-shift-forwards 99999})))

  (it "returns false when not in the beginning of periods"
      (should (not-any? #(sikaliiga/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 1 1200)))
      (should (not-any? #(sikaliiga/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 1201 2400)))
      (should (not-any? #(sikaliiga/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 2401 3600)))
      (should (not-any? #(sikaliiga/shift-forwards? {:seconds %} {:next-shift-forwards 99999}) (range 3601 3900))))

  (it "returns true when next-shift-forwards is reached"
      (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 10})))

  (it "returns true when next-shift-forwards is nil"
      (should (sikaliiga/shift-forwards? {:seconds 9} {})))

  (it "returns false when next-shift-forwards is not reached"
      (should-not (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20})))

  (it "returns true when not on power-play and current field is power-play"
      (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 4}))
      (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 5})))

  (it "returns true when not short-handed and current field is short-handed"
      (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 6}))
      (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 7})))

  (describe
    "when on power-play"
    (it "returns true when current field is not power-play"
        (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 0 :power-play? true}))))

  (describe
    "when short-handed"
    (it "returns true when current field is not short-handed"
        (should (sikaliiga/shift-forwards? {:seconds 10} {:next-shift-forwards 20 :current-field-forwards 0 :short-handed? true})))))

(describe
  "shift-defenders?"
  (it "returns true in the beginning of periods"
      (should (sikaliiga/shift-defenders? {:seconds 0} {:next-shift-defenders 99999}))
      (should (sikaliiga/shift-defenders? {:seconds 1200} {:next-shift-defenders 99999}))
      (should (sikaliiga/shift-defenders? {:seconds 2400} {:next-shift-defenders 99999}))
      (should (sikaliiga/shift-defenders? {:seconds 3600} {:next-shift-defenders 99999})))

  (it "returns false when not in the beginning of periods"
      (should (not-any? #(sikaliiga/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 1 1200)))
      (should (not-any? #(sikaliiga/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 1201 2400)))
      (should (not-any? #(sikaliiga/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 2401 3600)))
      (should (not-any? #(sikaliiga/shift-defenders? {:seconds %} {:next-shift-defenders 99999}) (range 3601 3900))))

  (it "returns true when next-shift-defenders is reached"
      (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 10})))

  (it "returns true when next-shift-defenders is nil"
      (should (sikaliiga/shift-defenders? {:seconds 9} {})))

  (it "returns false when next-shift-defenders is not reached"
      (should-not (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20})))

  (it "returns true when not on power-play and current field is power-play"
      (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 3}))
      (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 4})))

  (it "returns true when not short-handed and current field is short-handed"
      (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 5}))
      (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 6})))

  (describe
    "when on power-play"
    (it "returns true when current field is not power-play"
        (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 0 :power-play? true}))))

  (describe
    "when short-handed"
    (it "returns true when current field is not short-handed"
        (should (sikaliiga/shift-defenders? {:seconds 10} {:next-shift-defenders 20 :current-field-defenders 0 :short-handed? true})))))

(let [team {:match-team :home
            :players (util/key-by :id [{:id 1 :position ::player/goalie :status ::player/dressed}
                                       {:id 2 :position ::player/goalie :status ::player/dressed}
                                       {:id 3 :position ::player/defender :status ::player/dressed}
                                       {:id 4 :position ::player/defender :status ::player/dressed}
                                       {:id 5 :position ::player/defender :status ::player/dressed}
                                       {:id 6 :position ::player/defender :status ::player/dressed}
                                       {:id 7 :position ::player/defender :status ::player/dressed}
                                       {:id 8 :position ::player/defender :status ::player/dressed}
                                       {:id 9 :position ::player/left-wing :status ::player/dressed}
                                       {:id 10 :position ::player/left-wing :status ::player/dressed}
                                       {:id 11 :position ::player/left-wing :status ::player/dressed}
                                       {:id 12 :position ::player/left-wing :status ::player/dressed}
                                       {:id 13 :position ::player/center :status ::player/dressed}
                                       {:id 14 :position ::player/center :status ::player/dressed}
                                       {:id 15 :position ::player/center :status ::player/dressed}
                                       {:id 16 :position ::player/center :status ::player/dressed}
                                       {:id 17 :position ::player/right-wing :status ::player/dressed}
                                       {:id 18 :position ::player/right-wing :status ::player/dressed}
                                       {:id 19 :position ::player/right-wing :status ::player/dressed}
                                       {:id 20 :position ::player/right-wing :status ::player/dressed}])
            :fields {:forwards [{:index 0 :shift-length 40 :players [9 13 17]}
                                {:index 1 :shift-length 30 :players [10 14 18]}
                                {:index 2 :shift-length 20 :players [11 15 19]}
                                {:index 3 :shift-length 10 :players [12 16 20]}
                                {:index 4 :shift-length 60 :players [17 13 9]}
                                {:index 5 :shift-length 40 :players [18 14 10]}
                                {:index 6 :shift-length 60 :players [10 14]}
                                {:index 6 :shift-length 60 :players [11 15]}]}
            :field [nil nil nil]
            :posession :home}
      state {:seconds 0 :teams {:home team}}]

    (describe
      "shift-forwards"
      (it "returns first field in state team on period starts"
          (let [expected [nil nil (:players (first (get-in team [:fields :forwards])))]
                actual (get-in (sikaliiga/shift-forwards state team) [:teams :home :field])]
            (should= expected actual)))

      (it "returns state as was when this team doesn't have posession"
          (let [expected (assoc state :posession :away :seconds 1)]
            (should= expected (sikaliiga/shift-forwards expected team))))

      (describe
        "shifting when"
        (describe
          "team is on power-play"
          (it "returns next power-play field in state team"
              (pending "TODO")))))

    (describe
      "shift-defenders"
      (it "TODO"
          (pending "TODO"))))

