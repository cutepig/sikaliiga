(ns sikaliiga.team
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.util :refer [make-uuid key-by]]
            [sikaliiga.player :as player]
            [sikaliiga.field :as field]))

(s/def ::id uuid?)
(s/def ::name string?)
(s/def ::players (s/map-of uuid? ::player/player))

(s/def :field/index int?)
(s/def :field/shift-length int?)
(s/def :field/players (s/coll-of uuid?))
(s/def ::field (s/keys :req-un [:field/index :field/shift-length :field/players]))
(s/def :field/forwards (s/coll-of ::field))
(s/def :field/defenders (s/coll-of ::field))
(s/def :field/goalies :field/players)
(s/def ::fields (s/keys :req-un [:field/forwards :field/defenders :field/goalies]))

(s/def ::team (s/keys :req-un [::id ::name ::players ::fields]))

(defn auto-field-nth [goalies defenders left-wings centers right-wings n]
  [(:id (first goalies))
   (map :id (take 2 (drop (* 2 n) defenders)))
   [(:id (nth left-wings n)) (:id (nth centers n)) (:id (nth right-wings n))]])

(defn auto-forwards-nth [left-wings centers right-wings n]
  (if (some? right-wings)
    [(:id (nth left-wings n)) (:id (nth centers n)) (:id (nth right-wings n))]
    [(:id (nth left-wings n)) (:id (nth centers n))]))

(defn auto-defenders-nth [defenders n]
  (vec (map :id (take 2 (drop (* 2 n) defenders)))))

(defn auto-goalies [goalies]
  (map :id (take 2 goalies)))

(defn auto-fields [team]
  (let [players (vals (:players team))
        goalies (->> players (filter player/goalie?) (sort-by :defense) reverse)
        defenders (->> players (filter player/defender?) (sort-by :defense) reverse)
        left-wings (->> players (filter player/left-wing?) (sort-by :attack) reverse)
        centers (->> players (filter player/center?) (sort-by #(+ (:attack %) (:defense %))) reverse)
        right-wings (->> players (filter player/right-wing?) (sort-by :attack) reverse)
        pp-defenders (->> players (filter player/defender?) (sort-by :attack) reverse)
        sh-left-wings (->> players (filter player/left-wing?) (sort-by :defense) reverse)]
    {:forwards [{:index 0 :shift-length 40 :players (auto-forwards-nth left-wings centers right-wings 0)}
                {:index 1 :shift-length 30 :players (auto-forwards-nth left-wings centers right-wings 1)}
                {:index 2 :shift-length 20 :players (auto-forwards-nth left-wings centers right-wings 2)}
                {:index 3 :shift-length 10 :players (auto-forwards-nth left-wings centers right-wings 3)}
                ;; TODO: power-play specific attributes
                {:index 4 :shift-length 60 :players (auto-forwards-nth left-wings centers right-wings 0)}
                {:index 5 :shift-length 40 :players (auto-forwards-nth left-wings centers right-wings 1)}
                ;; TODO: short-handed specific attributes
                {:index 6 :shift-length 60 :players (auto-forwards-nth sh-left-wings centers nil 0)}
                {:index 7 :shift-length 40 :players (auto-forwards-nth sh-left-wings centers nil 1)}]
     :defenders [{:index 0 :shift-length 50 :players (auto-defenders-nth defenders 0)}
                 {:index 1 :shift-length 40 :players (auto-defenders-nth defenders 1)}
                 {:index 2 :shift-length 30 :players (auto-defenders-nth defenders 2)}
                 {:index 3 :shift-length 70 :players (auto-defenders-nth defenders 0)}
                 {:index 4 :shift-length 50 :players (auto-defenders-nth defenders 1)}
                 {:index 5 :shift-length 70 :players (auto-defenders-nth defenders 0)}
                 {:index 6 :shift-length 50 :players (auto-defenders-nth defenders 1)}]
     :goalies (auto-goalies goalies)}))

(defn make-test-team [name min-skill max-skill]
  (let [players [(player/make-player :position ::player/goalie :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/goalie :min-skill min-skill :max-skill max-skill)

                 (player/make-player :position ::player/defense :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/defense :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/defense :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/defense :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/defense :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/defense :min-skill min-skill :max-skill max-skill)

                 (player/make-player :position ::player/left-wing :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/left-wing :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/left-wing :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/left-wing :min-skill min-skill :max-skill max-skill)

                 (player/make-player :position ::player/center :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/center :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/center :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/center :min-skill min-skill :max-skill max-skill)

                 (player/make-player :position ::player/right-wing :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/right-wing :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/right-wing :min-skill min-skill :max-skill max-skill)
                 (player/make-player :position ::player/right-wing :min-skill min-skill :max-skill max-skill)]

        team {:id (make-uuid)
              :name name
              :players (key-by :id players)}]

    ;; Assign fields automatically
    (s/assert ::team (assoc team :fields (auto-fields team)))))

