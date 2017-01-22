(ns sikaliiga.ui.core
  (:require [clojure.string :as string]
            [reagent.core :as r]
            [sikaliiga.ui.store :refer [make-dispatch reducer]]
            [sikaliiga.util :refer [str-left-pad-num]]
            [sikaliiga.simulation.match.core :as match]
            [sikaliiga.team :refer [make-test-team]]
            [sikaliiga.field :as field]))

(defmethod reducer :start-match [state _]
  (-> state
      (assoc :ui/state :ui-state/match)
      (update :simulation/state #(apply match/prepare-state (:simulation/teams state)))
      (assoc-in [:simulation/state :seconds] 0)))

(defmethod reducer :simulate-second [state]
  (-> state
    (update :simulation/state match/simulate-second)
    (update-in [:simulation/state :seconds] inc)))

(defmethod reducer :simulate-match [state]
  (update state :simulation/state match/simulate-match))

(defmethod reducer :simulate-ot [state]
  (-> state
      (assoc-in [:simulation/state :seconds] 3600)
      (update :simulation/state match/simulate-ot)))

(defmethod reducer :simulate-shootouts [state]
  (update state :simulation/state match/simulate-shootouts))

(defmethod reducer :reset-state [state]
  (assoc state :ui/state :ui-state/init))

(defn clock-view [seconds]
  [:span.clock
    (str (str-left-pad-num 2 (js/Math.floor (/ seconds 60)))
         ":"
         (str-left-pad-num 2 (mod seconds 60)))])

(defn game-clock-view [state]
  (let [seconds (:seconds state)]
    [:span.game-clock
      (clock-view seconds)
      (cond
        (:shootouts? state)
        (str " (VL " (get-in state [:teams :home :shootouts]) " - " (get-in state [:teams :away :shootouts]) ")")

        (> seconds 3600) " (JA)"
        :else "")]))

(defn scoreboard-view [state]
  [:div.scoreboard
    (str (get-in state [:teams :home :name])
         " - "
         (get-in state [:teams :away :name])
         " "
         (get-in state [:teams :home :goals])
         " - "
         (get-in state [:teams :away :goals])
         " ("
         (get-in state [:teams :home :sog]) "/" (get-in state [:teams :home :shots])
         " - "
         (get-in state [:teams :away :sog]) "/" (get-in state [:teams :away :shots])
         ")")])

(defn field-player-view [player]
  ;; TODO: status class
  [:div.field-player
   [:span.field-player__name (get player :name "-")]
   [:span.field-player__props
    (str (int (* 100 (:attack player))) "/" (int (* 100 (:defense player))))]
   [:span.field-player__props.field-player__props--original
    (str "(" (int (* 100 (:original-attack player))) "/" (int (* 100 (:original-defense player))) ")")]])

(defn field-players-view [team players]
  [:ul.field-players
     (for [id players
           :when (some? id)
           :let [player (get-in team [:players id])]]
       ^{:key id} [:li.field-players__player [field-player-view player]])])

(defn field-view [team]
  [:ul.field
   [:li.field__forwards
    [field-players-view team (field/get-forwards (:field team))]]
   [:li.field__defenders
    [field-players-view team (field/get-defenders (:field team))]]
   [:li.field__goalie
    [field-players-view team [(field/get-goalie (:field team))]]]])

(defn penalty-box-view [state team]
  [:div.penalty-box
    [:h5 "Jäähyboksi"]
    [:ul
      (for [[id {:keys [time length]}] (:penalty-box team)
            :let [player (get-in team [:players id])
                  seconds (:seconds state)]]
        ^{:key id}
        [:li.penalty-box__sitter
          (str (:name player) " ") [clock-view (- length (- seconds time))]])]])

(defn simulation-team-view [state team]
  ;; TODO: Class on posession
  [:div.simulation-team
   [:h4 (str (:name team) (if (= (:posession state) (:match-team team)) "*"))]
   [field-view team]
   [penalty-box-view state team]])

(defn get-team-name [team]
  (string/upper-case (subs (string/replace (:name team) " " "") 0 3)))

(defn simulation-event-shootout-goal-view [state event]
    (let [[_ _ round shooter-match-team shooter-id _ _] event
          team (get-in state [:teams shooter-match-team])
          shooter (get-in team [:players shooter-id])]
      (str "SO #" (inc round) " GOAL!: " (:name shooter) "(" (get-team-name team) ") ")))

(defn simulation-event-shootout-miss-view [state event]
    (let [[_ _ round shooter-match-team shooter-id _ _] event
          team (get-in state [:teams shooter-match-team])
          shooter (get-in team [:players shooter-id])]
      (str "SO #" (inc round) " Miss: " (:name shooter) "(" (get-team-name team) ") ")))

(defn simulation-event-face-off-view [state event]
  (let [[_ sec winner-match-team winner-player-id loser-match-team loser-player-id] event
        winner-team (get-in state [:teams winner-match-team])
        winner-player (get-in winner-team [:players winner-player-id])
        loser-team (get-in state [:teams loser-match-team])
        loser-player (get-in loser-team [:players loser-player-id])]
    (str "Face-off: "
         (:name winner-player) "(" (get-team-name winner-team) ") - "
         (:name loser-player) "(" (get-team-name loser-team) ")")))

(defn simulation-event-shot-view [state event]
  (let [[_ sec match-team player-id] event
        team (get-in state [:teams match-team])
        player (get-in team [:players player-id])]
    (str "Shot: " (:name player) "(" (get-team-name team) ")")))

(defn simulation-event-sog-view [state event]
  (let [[_ sec match-team player-id] event
        team (get-in state [:teams match-team])
        player (get-in team [:players player-id])]
    (str "SoG: " (:name player) "(" (get-team-name team) ")")))

(defn simulation-event-goal-view [state event]
    (let [[_ sec goal-match-team shooter-id _ _ type] event
          team (get-in state [:teams goal-match-team])
          shooter (get-in team [:players shooter-id])
          type-s (case type :power-play "(YV) " :short-handed "(AV) " "")]
      (str "GOAL!: " type-s (:name shooter) "(" (get-team-name team) ") ")))

(defn simulation-event-penalty-view [state event]
    (let [[_ sec match-team player-id length] event
          team (get-in state [:teams match-team])
          player (get-in team [:players player-id])]
      (str (int (/ length 60)) " min: " (:name player) "(" (get-team-name team) ")")))

(defn simulation-event-view [state event]
  [:div.simulation-event
    [clock-view (second event)]
    (str " - ")
    [:span
     (case (first event)
        :face-off (simulation-event-face-off-view state event)
        :shot (simulation-event-shot-view state event)
        :sog (simulation-event-sog-view state event)
        :goal (simulation-event-goal-view state event)
        :penalty (simulation-event-penalty-view state event)
        :shootout-miss (simulation-event-shootout-miss-view state event)
        :shootout-goal (simulation-event-shootout-goal-view state event)
        (str (name (first event)) (clojure.string/join ", " (rest (rest event)))))]])


(defn simulation-events-view [state]
  [:ul.simulation-events
   (reverse
     (for [idx (range (count (:events state)))
           :let [event (nth (:events state) idx)]]
       ^{:key idx} [:li.simulation-events__event [simulation-event-view state event]]))])

(defn simulation-view [state dispatch]
  (let [simulation-state (:simulation/state @state)]
    [:div.simulation
     [:div
      [:button {:on-click #(dispatch [:simulate-second])} "Simulate second"]
      [:button {:on-click #(dispatch [:simulate-match])} "Simulate match"]
      [:button {:on-click #(dispatch [:start-match])} "Reset match"]
      [:button {:on-click #(dispatch [:reset-state])} "Back"]]
     [:div
      [:button {:on-click #(dispatch [:simulate-ot])} "(Simulate OT)"]
      [:button {:on-click #(dispatch [:simulate-shootouts])} "(Simulate shootouts)"]]
     [:div.simulation__second (game-clock-view simulation-state)]
     [:div.simulation__scoreboard (scoreboard-view simulation-state)]
     [:ul.simulation__teams
      [:li.simulation__team [simulation-team-view simulation-state (get-in simulation-state [:teams :home])]]
      [:li.simulation__team [simulation-team-view simulation-state (get-in simulation-state [:teams :away])]]]
     [:div.simulation__events [simulation-events-view simulation-state]]]))

(defn start-match-view [state dispatch]
  (if (= (count (:simulation/teams @state)) 2)
    [:div.start-match
     [:button {:on-click #(dispatch [:start-match])} "Start game"]]))

(defn team-player [player]
  [:div.team-player (str (:name player) " "
                         (int (* 100 (:attack player))) "/"
                         (int (* 100 (:defense player))))])

(defn forwards-line-name [index]
  (cond
    (and (some? index) (< index 4)) (str index)
    (and (>= index 4) (< index 6)) "YV"
    (and (>= index 6) (< index 8)) "AV"
    :else "?"))

(defn defenders-line-name [index]
  (cond
    (and (some? index) (< index 3)) (str index)
    (and (>= index 3) (< index 5)) "YV"
    (and (>= index 5) (< index 7)) "AV"
    :else "?"))

(defn team-line [team line]
  [:ul.team-line
    {:style #js {:display "inline-block"}}
    (for [id (:players line)
          :let [player (get-in team [:players id])]]
      ^{:key id} [:li.team-line__player [team-player player]])])

(defn team-roster-view [team]
  [:div.team-roster
   [:h4 "Forwards"]
   [:ul.team__forward-lines
    (for [forwards (get-in team [:fields :forwards])]
      ^{:key (:index forwards)}
      [:li.team__forward-line
       [:b (forwards-line-name (:index forwards))]
       [team-line team forwards]])]
   [:h4 "Defenders"]
   [:ul.team__defense-lines
    (for [defenders (get-in team [:fields :defenders])]
      ^{:key (:index defenders)}
      [:li.team__defense-line
       [:b (defenders-line-name (:index defenders))]
       [team-line team defenders]])]
   [:h4 "Goalies"]
   [team-line team {:players (get-in team [:fields :goalies])}]])

(defn calculate-team-skill [team key]
  (->> (vals (:players team))
       (map key)
       (reduce + 0)
       (#(* 100 (/ % (count (:players team)))))))

(defn team-view [team]
  [:div.team
   [:h3 (str (:name team) " (" (:id team) ")")]
   [:table.team__stats
    [:tbody [:tr [:th "Attack"] [:td (int (calculate-team-skill team :attack))]]
            [:tr [:th "Defense"] [:td (int (calculate-team-skill team :defense))]]]]
   [:div.team__roster [team-roster-view team]]])

(defn teams-view [teams]
  [:ul.teams-view
   (for [team teams]
     ^{:key (:id team)} [:li.teams-view__team [team-view team]])])

(defmethod reducer :assoc-in [state [_ ks value]]
  (assoc-in state ks value))

(defn team-generator-settings-view [state dispatch name]
  [:div.team-generator-settings
   [:label (str name " min skill")
    (let [name (str name "-min-skill")]
      [:input {:type :number
               :name name
               :value (get-in @state [:ui/generate-teams name] 50)
               :on-change #(dispatch [:assoc-in [:ui/generate-teams name] (.-currentTarget.value %)])}])]
   [:label (str name " max skill")
    (let [name (str name "-max-skill")]
      [:input {:type :number
               :name name
               :value (get-in @state [:ui/generate-teams name] 80)
               :on-change #(dispatch [:assoc-in [:ui/generate-teams name] (.-currentTarget.value %)])}])]])

(defn teams-generator-view [state dispatch]
  [:div.teams-generator
   [:h2 "Generate teams"]
   [team-generator-settings-view state dispatch "team-a"]
   [team-generator-settings-view state dispatch "team-b"]
   [:button {:on-click #(dispatch [:assoc-in
                                   [:simulation/teams]
                                   [(make-test-team "A Tiimi"
                                                    (get-in @state [:ui/generate-teams "team-a-min-skill"] 50)
                                                    (get-in @state [:ui/generate-teams "team-a-max-skill"] 80))
                                    (make-test-team "B Tiimi"
                                                    (get-in @state [:ui/generate-teams "team-b-min-skill"] 50)
                                                    (get-in @state [:ui/generate-teams "team-b-max-skill"] 80))]])}
    "Generate"]])

(defn init-view [state dispatch]
  [:div.init
   [:div.app-teams-generator
    [teams-generator-view state dispatch]]
   [start-match-view state dispatch]
   [:div.app-teams
    [teams-view (:simulation/teams @state)]]])

(defonce app-state (r/atom {:ui/state :ui-state/init
                            :ui/generate-teams {}
                            :simulation/teams []
                            :simulation/state nil}))

;; We need some way to abstract this away and make some
;; `with-dispatch` decorator for components
(def dispatch (make-dispatch app-state))

(defn app-view [state]
  [:div.app
   (if (= :ui-state/init (:ui/state @state))
     [init-view state dispatch]
     [simulation-view state dispatch])])
