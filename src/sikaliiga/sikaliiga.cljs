(ns sikaliiga.sikaliiga)

(defn rnd [min max] (+ (rand (- max min)) min))
;; Random int from min (inclusive) to max (exclusive!)
;; @example Get random int in 0-99 (inclusive)
;;   `(irnd 0 100)`
(defn irnd [min max] (int (rnd min max)))

;;
;; Player creation
(def names-dict {:fi {:first-names
                      ["Antti" "Juha" "Keijo" "Matti" "Teppo" "Teemu"]
                      :last-names
                      ["Nieminen" "Toivonen" "Aaltonen" "Keinänen" "Pasanen" "Peltonen"]}
                 :us {:first-names
                      ["Andrew" "Matthew" "Tony" "Matt" "John" "Taylor" "Corey"]
                      :last-names
                      ["Johnson" "Nelson" "Simpson" "Martin" "Connor" "Lewis" "Green" "Moore"]}})

;; Creates a random name with optionally given locale and name dictionary
;; @example Default locale (fi) and builtin name dictionary
;;  `(random-name)`
;; @example Use locale us
;;  `(random-name {:locale :us})`
(defn random-name
  [& {:keys [locale dict] :or {locale :fi dict names-dict}}]
  (str (rand-nth (get-in names-dict [locale :last-names]))
       ", "
       (rand-nth (get-in names-dict [locale :first-names]))))


(defn forward-position? [pos]
  (contains? #{:left-wing :center :right-wing} pos))

(defn defense-position? [pos]
  (= pos :defense ))

(defn goalie-position? [pos]
  (= pos :goalie))

(defn defense-or-goalie-position? [pos]
  (or (defense-position? pos) (goalie-position? pos)))

(defn goalie? [player] (goalie-position? (:position player)))
(defn defender? [player] (defense-position? (:position player)))
(defn forward? [player] (forward-position? (:position player)))

;; TODO: Salary, contract, fitness, motivation, character/charisma, leadership
(defn make-player
  [& {:keys [locale position min-skill max-skill] :or {locale :fi}}]
  ;; Calculating potential this way ain't gonna cut it.. While the idea of lower potential than current skill
  ;; is nice, this can generate some wild differences specially in non-position skill
  (let [[attack attack-potential] (if (forward-position? position)
                                    [(irnd min-skill max-skill) (irnd min-skill 100)]
                                    [(irnd 1 max-skill) (irnd 1 max-skill)])
        [defense defense-potential] (if (defense-or-goalie-position? position)
                                     [(irnd min-skill max-skill) (irnd min-skill 100)]
                                     [(irnd 1 max-skill) (irnd 1 max-skill)])]
    {:id (str (random-uuid))
     :age (irnd 18 35)
     :name (random-name :locale locale)
     :locale locale
     :position position
     :attack attack
     :attack-potential attack-potential
     :defense defense
     :defense-potential defense-potential}))

;; Fields
(defn field-goalie [field]
  (first (:players field)))

(defn field-defenders [field]
  (second (:players field)))

(defn field-defender [field n]
  (nth (field-defenders field) n))

(defn field-forwards [field]
  (nth (:players field) 2))

(defn field-left-wing [field]
  (first (field-forwards field)))

(defn field-center [field]
  (second (field-forwards field)))

(defn field-right-wing [field]
  (nth (field-forwards field) 2))

(defn field-extra-forward [field]
  (nth (field-forwards field) 3))

(defn calculate-field-attack [field]
  (/ (reduce + 0 (map :attack (field-forwards field))) 3))

(defn calculate-field-defense [field]
  (/ (reduce + 0 (map :defense (field-defenders field))) 2))

(defn calculate-field-goalie [field]
  (or (:defense (first field)) 0))

(defn player-by-id [team id]
  (some #(if (= (:id %) id) %) (:players team)))

;; `field-out` prepared field consisting of actual players instead of id's.
;; Map `field-out` stats from `team` back to `new-team`.
;; Prepare `field-in` consisting of actual players instead of id's
;; Return `[new-team field-in]`
(defn shift-field-forwards [secs team field-out]
  ;; TODO: Powerplay
  ;; TODO: Shorthand
  ;; TODO: Skip injured players or players on penalty
  (let [next-index (mod (inc (or (:index-forwards field-out) -1)) 4)
        next-field (get-in team [:fields next-index])
        forwards (map #(player-by-id team %) (field-forwards next-field))
        field-in (-> field-out
                     (assoc-in [:players 2] forwards)
                     (assoc :shift-forwards (:shift-forwards next-field))
                     ;; TODO: Slight variance to this
                     (assoc :next-shift-forwards (+ secs (:shift-forwards next-field)))
                     (assoc :index-forwards next-index))]
    [team field-in]))

;; Initial field can be set by giving `nil` as `field-out`
(defn shift-field-defenders [secs team field-out]
  ;; TODO: Powerplay
  ;; TODO: Shorthand
  ;; TODO: Skip injured players or players on penalty
  (let [next-index (mod (inc (or (:index-defenders field-out) -1)) 3)
        next-field (get-in team [:fields next-index])
        defenders (map #(player-by-id team %) (field-defenders next-field))
        field-in (-> field-out
                     (assoc-in [:players 1] defenders)
                     (assoc :shift-defenders (:shift-defenders next-field))
                     ;; TODO: Slight variance to this
                     (assoc :next-shift-defenders (+ secs (:shift-defenders next-field)))
                     (assoc :index-defenders next-index))]
    [team field-in]))

(defn pull-field-goalie [secs team field-out]
  ;; Null goalie and return
  [team (assoc-in field-out [:players 0] nil)])

(defn shift-forwards? [secs field]
  (>= secs (:next-shift-forwards field)))

(defn shift-defenders? [secs field]
  (>= secs (:next-shift-defenders field)))

(defn auto-field-nth [goalies defenders forwards n]
  [(:id (first goalies))
   (map :id (take 2 (drop (* 2 n) defenders)))
   (map :id (take 3 (drop (* 3 n) defenders)))])

(defn auto-fields [team]
  (let [players (:players team)
        goalies (->> players (filter goalie?) (sort-by :defense) reverse)
        defenders (->> players (filter defender?) (sort-by :defense) reverse)
        forwards (->> players (filter forward?) (sort-by :attack) reverse)]
    ;; FIXME: Handle left-, right-wings and center correctly
    [{:index 0 :shift-forwards 40 :shift-defenders 50 :players (auto-field-nth goalies defenders forwards 0)}
     {:index 1 :shift-forwards 30 :shift-defenders 40 :players (auto-field-nth goalies defenders forwards 1)}
     {:index 2 :shift-forwards 20 :shift-defenders 30 :players (auto-field-nth goalies defenders forwards 2)}
     {:index 3 :shift-forwards 20 :players (auto-field-nth goalies defenders forwards 3)}]))

(defn make-test-team [min-skill max-skill]
  (let [players [(make-player :position :goalie :min-skill min-skill :max-skill max-skill)

                 (make-player :position :defense :min-skill min-skill :max-skill max-skill)
                 (make-player :position :defense :min-skill min-skill :max-skill max-skill)
                 (make-player :position :defense :min-skill min-skill :max-skill max-skill)
                 (make-player :position :defense :min-skill min-skill :max-skill max-skill)
                 (make-player :position :defense :min-skill min-skill :max-skill max-skill)
                 (make-player :position :defense :min-skill min-skill :max-skill max-skill)

                 (make-player :position :left-wing :min-skill min-skill :max-skill max-skill)
                 (make-player :position :left-wing :min-skill min-skill :max-skill max-skill)
                 (make-player :position :left-wing :min-skill min-skill :max-skill max-skill)
                 (make-player :position :left-wing :min-skill min-skill :max-skill max-skill)

                 (make-player :position :center :min-skill min-skill :max-skill max-skill)
                 (make-player :position :center :min-skill min-skill :max-skill max-skill)
                 (make-player :position :center :min-skill min-skill :max-skill max-skill)
                 (make-player :position :center :min-skill min-skill :max-skill max-skill)

                 (make-player :position :right-wing :min-skill min-skill :max-skill max-skill)
                 (make-player :position :right-wing :min-skill min-skill :max-skill max-skill)
                 (make-player :position :right-wing :min-skill min-skill :max-skill max-skill)
                 (make-player :position :right-wing :min-skill min-skill :max-skill max-skill)]

        team {:id (random-uuid)
              :name (str "Test team (" min-skill " - " max-skill ")")
              ;; FIXME: Should players be stored by id?
              :players players}]

    ;; Assign fields automatically
    (assoc team :fields (auto-fields team))))

;;
;; Simulation functions

(defn power-play? [] (= 0 (rand-int 5)))

;; NOTE: This old version had power-play affecting the situation
;; TODO: Port this back to the new version
;; TODO: Also add in short-handed
(defn -shot? [a b power-play?]
  (if power-play?
    (> (rand (:power-play a)) (rand (:short-handed b)))
    (> (rand (:attack a)) (rand (:defense b)))))

(defn -goal? [a b power-play?]
  (if power-play?
    (> (rand (:power-play a)) (+ (rand (* 30 (:goalie b))) (* 15 (:goalie b))))
    (> (rand (:attack a)) (+ (rand (:goalie b)) (/ (:defense b) 3)))))

;; TODO: Tweak this constant
(def *mean-shots-per-sec* (/ 40 3600))
(defn shot? [attack defense]
  (< (rand)
     (+ (* (- attack defense) *mean-shots-per-sec*) *mean-shots-per-sec*)))

;; TODO: Tweak this constant
(def *mean-block-probability* 0.2)
(defn blocked? [attack defense]
  (< (rand)
     (+ (* (- defense attack) *mean-block-probability*) *mean-block-probability*)))

;; TODO: Tweak this constant
(def *mean-miss-probability* 0.2)
(defn missed? [attack defense]
  (< (rand)
     (+ (* (- (* defense 0.5) attack) *mean-miss-probability*) *mean-miss-probability*)))

;; TODO: Tweak this constant
;; TODO: Factor in D to affect shot quality?
(def *mean-goal-probability* 0.09)
(defn goal? [attack goalie]
  (< (rand)
     (+ (* (- attack goalie) *mean-goal-probability*) *mean-goal-probability*)))

(defn add-shot [state team]
  (update-in state [:teams team :shots] inc))

(defn add-block-against [state team]
  (update-in state [:teams team :blocked] inc))

(defn add-block [state team]
  (update-in state [:teams team :blocks] inc))

(defn add-miss [state team]
  (update-in state [:teams team :missed] inc))

(defn add-goal [state team]
  (update-in state [:teams team :goals] inc))

(defn add-goal-against [state team]
  (update-in state [:teams team :goals-against] inc))

(defn simulate-shots [state]
  (let [home (get-in state [:teams :home])
        visitor (get-in state [:teams :visitor])]
    ;; Shot should be affected by whole field A D
    (if (not (shot? (:attack home) (:defense visitor)))
      state
      ;; Block should be affected by shooter A (?) and opponent field D
      ;; On the other hand field A determines the quality of the shot
      (if (blocked? (:attack home) (:defense visitor))
        ;; Initial idea was to return state and list of events which is still viable:
        ;; [state [add-shot team] [add-block-against team] [add-block opponent]]
        ;; Just went with threading to macro to get immediate result
        ;; Returning list of events allows us to give important meta-data to show full results
        (-> state (add-shot :home) (add-block-against :home) (add-block :visitor))
        ;; Miss should be affected by shooter A
        (if (missed? (:attack home) (:defense visitor))
          (-> state (add-shot :home) (add-miss :home))
          ;; Goal should be affected by shooter A and opponent G
          (if (goal? (:attack home) (:goalie visitor))
            (-> state (add-shot :home) (add-goal :home) (add-goal-against :visitor))
            (-> state (add-shot :home))))))))

;; FIXME: Why isn't the state updated?!
(defn simulate-second [state sec]
  (simulate-shots state))

;; Team looked like this:
(defn simulate-game [home visitor]
  (let [state {:teams {:home home :visitor visitor}}
        seconds (range 3600)
        foo (reduce simulate-second state seconds)]
    foo))

;; Starting to look good
(def team-a (make-test-team 50 75))
(def team-b (make-test-team 55 80))

(defn calculate-attack [team]
  (let [attackers (filter #(forward-position? (:position %)) (:players team))]
    (/ (reduce + (map :attack attackers)) (or (count attackers) 1))))

(defn calculate-defense [team]
  (let [defenders (filter #(defense-position? (:position %)) (:players team))]
    (/ (reduce + (map :defense defenders)) (or (count defenders) 1))))

(defn calculate-goalie [team]
  (let [goalies (filter #(goalie-position? (:position %)) (:players team))]
    (/ (reduce + (map :defense goalies)) (or (count goalies) 1))))

(defn calculate-power-play [team])

(defn calculate-short-handed [team])

(defn prepare-player [player]
  (assoc player :attack (/ (:attack player) 100)
                :defense (/ (:defense player) 100)))

;; In the future, we team-d*on't do teateam-m*-level attack/defense/goalie values because we will simulate fields
;; `{:a .75 :d .75 :g .85 :-ashots 0 :blocked-a 0 :missed 0 :goals 0 :team-bs 0}`
(defn prepare-team [team]
  (merge team
         ;; TODO: Get rid of attack values
         {:players (map prepare-player (:players team))
          :shots 0
          :blocked 0
          :missed 0
          :goals 0
          :goals-against 0
          :blocks 0}))

(simulate-game (prepare-team team-a) (prepare-team team-b))

;; In-game fields shall look like this
{:players []
 :shift-forwards 40
 :index-forwards 0
 :next-shift-forwards 40
 :shift-defenders 50
 :index-defenders 0
 :next-shift-defenders 50}
