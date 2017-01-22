(ns sikaliiga.player
  (:require [clojure.spec :as s]
            [sikaliiga.util :refer [make-uuid rnd irnd]]))

;; Spec
(s/def ::id uuid?)
(s/def ::age integer?)
(s/def ::name string?)
(s/def ::locale keyword?)
(s/def ::position #{::goalie ::defense ::left-wing ::center ::right-wing})
(s/def ::attack number?)
(s/def ::attack-potential number?)
(s/def ::defense number?)
(s/def ::defense-potential number?)
(s/def ::fitness number?)
(s/def ::morale number?)
(s/def ::status #{::dressed ::injured ::penalty ::bench ::match-penalty})

(s/def ::player (s/keys :req-un [::id ::age ::name ::locale ::position ::attack
                                 ::attack-potential ::defense ::defense-potential
                                 ::fitness ::morale ::status]))

(defn by-id [players id]
  (some #(if (= (:id %) id) %) players))

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

;; TODO: Tests
(defn calculate-match-attack [team player]
  (* (:attack player) (* 0.5 (+ (:morale player) (:fitness player)))))

(defn calculate-match-defense [team player]
  (* (:defense player) (* 0.5 (+ (:morale player) (:fitness player)))))

(defn calculate-match-skill [team player]
  (* 0.5 (+ (calculate-match-attack team player)
            (calculate-match-defense team player))))

(defn left-wing-position? [pos]
  (= pos ::left-wing))

(defn center-position? [pos]
  (= pos ::center))

(defn right-wing-position? [pos]
  (= pos ::right-wing))

(defn forward-position? [pos]
  (contains? #{::left-wing ::center ::right-wing} pos))

(defn defense-position? [pos]
  (= pos ::defense ))

(defn goalie-position? [pos]
  (= pos ::goalie))

(defn defense-or-goalie-position? [pos]
  (or (defense-position? pos) (goalie-position? pos)))

(defn goalie? [player] (goalie-position? (:position player)))
(defn defender? [player] (defense-position? (:position player)))
(defn forward? [player] (forward-position? (:position player)))
(defn left-wing? [player] (left-wing-position? (:position player)))
(defn center? [player] (center-position? (:position player)))
(defn right-wing? [player] (right-wing-position? (:position player)))

(defn dressed? [player] (= ::dressed (:status player)))
(defn injured? [player] (= ::injured (:status player)))
(defn penalty? [player] (= ::penalty (:status player)))
(defn bench? [player] (= ::bench (:status player)))
(defn match-penalty? [player] (= ::match-penalty (:status player)))

;; TODO: Salary, contract, fitness, morale, character/charisma, leadership
(defn make-player
  [& {:keys [locale position min-skill max-skill] :or {locale :fi}}]
  ;; Calculating potential this way ain't gonna cut it.. While the idea of lower potential than current skill
  ;; is nice, this can generate some wild differences specially in non-position skill
  (let [min-skill* (/ min-skill 100)
        max-skill* (/ max-skill 100)
        other-skill (rnd 0.01 max-skill*)
        [attack attack-potential] (if (forward-position? position)
                                    [(rnd min-skill* max-skill*) (rnd min-skill* 1)]
                                    [other-skill (rnd other-skill max-skill*)])
        [defense defense-potential] (if (defense-or-goalie-position? position)
                                      [(rnd min-skill* max-skill*) (rnd min-skill* 1)]
                                      [other-skill (rnd other-skill max-skill*)])]
    {:id (make-uuid)
     :age (irnd 18 35)
     :name (random-name :locale locale)
     :locale locale
     :position position
     :attack attack
     :attack-potential attack-potential
     :defense defense
     :defense-potential defense-potential
     :fitness 1
     :morale 1
     :character (rand)
     :leadership (rand)
     :status ::dressed}))

(defn make-test-player [id attack defense position status]
    {:id id :position position :status status :age 20 :name "Teemu mä itken" :locale :fi
     :attack attack :defense defense :fitness 1 :morale 1 :attack-potential attack
     :defense-potential defense :on-ice? false
     :toc 0 :face-offs 0 :face-off-wins 0 :shots 0 :blocked 0
     :missed 0 :goals 0 :goals-against 0 :blocks 0})
