(ns sikaliiga.simulation.match.specs
  (:require [clojure.spec :as s :include-macros true]
            [sikaliiga.player :as player]
            [sikaliiga.field :as field]
            [sikaliiga.team :as team]))

(s/def :player/team uuid?)  ;; TODO: Add this back in, temporarily removed because of tests dont def this
(s/def :player/shots integer?)
(s/def :player/blocked integer?)
(s/def :player/missed integer?)
(s/def :player/goals integer?)
(s/def :player/goals-against integer?)
(s/def :player/blocks integer?)
(s/def ::player (s/merge ::player/player
                         (s/keys :req-un [:player/shots :player/blocked :player/missed
                                          :player/goals :player/goals-against :player/blocks])))

(s/def :team/field ::field/field)
(s/def :team/players (s/map-of uuid? ::player))
(s/def :team/match-team #{:home :away})
(s/def :team/current-field-forwards (s/nilable integer?))
(s/def :team/current-field-defenders (s/nilable integer?))
(s/def :team/power-play? boolean?)
(s/def :team/short-handed? boolean?)

(s/def :penalty-sitter/time integer?)
(s/def :penalty-sitter/length integer?)
(s/def :team/penalty-sitter (s/keys :req-un [:penalty-sitter/time :penalty-sitter/length]))
(s/def :team/penalty-box (s/map-of ::player/id :team/penalty-sitter))

(s/def ::team (s/merge ::team/team
                       (s/keys :req-un [:team/field :team/players :team/match-team
                                        :team/current-field-forwards :team/current-field-defenders]
                               :opt-un [:team/power-play? :team/short-handed?])))

(s/def ::seconds integer?)
(s/def ::shootouts boolean?)
(s/def ::posession keyword?)
(s/def ::overtime boolean?)
(s/def ::home ::team)
(s/def ::away ::team)
(s/def ::teams (s/keys :req-un [::home ::away]))
(s/def ::state (s/keys :req-un [::seconds ::teams]
                       :opt-un [::posession ::overtime ::shootouts?]))

