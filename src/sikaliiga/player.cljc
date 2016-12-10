(ns sikaliiga.player
  (:require [clojure.spec :as s]
            [sikaliiga.util :refer [make-uuid rnd irnd]]))

;; Spec
(s/def ::id uuid?)
(s/def ::age integer?)
(s/def ::name string?)
(s/def ::locale keyword?)
(s/def ::position #(contains? [::goalie ::defense ::left-wing ::center ::right-wing] %))
(s/def ::attack number?)
(s/def ::attack-potential number?)
(s/def ::defense number?)
(s/def ::defense-potential number?)
(s/def ::fitness number?)
(s/def ::morale number?)
(s/def ::status #(contains? [::dressed ::injured ::penalty ::bench ::match-penalty] %))

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

(defn calculate-match-attack [team player]
  ;; FIXME: Proper mapping including team attributes, personal fitness and morale
  (* (:attack player) (* 0.5 (+ (:morale player) (:fitness player)))))

(defn calculate-match-defense [team player]
  ;; FIXME: Proper mapping including team attributes, personal fitness and morale
  (* (:defense player) (* 0.5 (+ (:morale player) (:fitness player)))))

(defn calculate-match-skill [team player]
  ;; FIXME: Proper mapping including team attributes, personal fitness and morale
  (+ (calculate-match-attack team player)
     (calculate-match-defense team player)))

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
     :status ::dressed}))
