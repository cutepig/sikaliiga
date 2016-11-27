(ns sikaliiga.team
  (:require [clojure.spec :as s]
            [sikaliiga.util :as util]
            [sikaliiga.player :as player]
            [sikaliiga.field :as field]))

(s/def ::id string?)
(s/def ::name string?)
(s/def ::players (s/map-of string? ::player/player))

(s/def ::team (s/keys :req-un [::id ::name ::players]))

(defn make-test-team [min-skill max-skill]
  (let [players [(player/make-player :position ::player/goalie :min-skill min-skill :max-skill max-skill)

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

        team {:id (str (random-uuid))
              :name (str "Test team (" min-skill " - " max-skill ")")
              ;; FIXME: Should players be stored by id?
              :players (util/key-by :id players)}]

    ;; Assign fields automatically
    (assoc team :fields (field/auto-fields team))))
