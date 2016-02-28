(ns amiss.data)

(defonce court-values
  {:princess  8
   :minister  7
   :general   6
   :wizard    5
   :priestess 4
   :knight    3
   :clown     2
   :soldier   1})

(defonce court-deck
  (flatten
    [:princess
     :minister
     :general
     (repeat 2 :wizard)
     (repeat 2 :priestess)
     (repeat 2 :knight)
     (repeat 2 :clown)
     (repeat 5 :soldier)]))

;; Ruleset names
(defmulti card-names (fn [ruleset] ruleset))
(defmethod card-names :original [_]
  {:princess "Princess"
   :minister "Minister"
   :general "General"
   :wizard "Wizard"
   :priestess "Priestess"
   :knight "Knight"
   :clown "Clown"
   :soldier "Soldier"})
(defmethod card-names :tempest [_]
  {:princess "Princess"
   :minister "Countess"
   :general "King"
   :wizard "Prince"
   :priestess "Handmaid"
   :knight "Baron"
   :clown "Priest"
   :soldier "Guard"})
(defmethod card-names :loot [_]
  {:princess "Loot!"
   :minister "Torbonium Dragon"
   :general "Dread Gazebo"
   :wizard "Net Troll"
   :priestess "Wishing Ring"
   :knight "Duck of Doom"
   :clown "Maul Rat"
   :soldier "Potted Plant"})
(defmethod card-names :santa [_]
  {:princess "Santa Claus"
   :minister "Mrs. Claus"
   :general "Kif"
   :wizard "Toys"
   :priestess "Snowman"
   :knight "Gingerbread Man"
   :clown "Reindeer"
   :soldier "Krampus"})
(defmethod card-names :adventure-time [_]
  {:princess "Princess"
   :minister "Lady"
   :general "Wizard"
   :wizard "Hero"
   :priestess "Companion"
   :knight "Gossip"
   :clown "Royal Subject"
   :soldier "Guard"})
(defmethod card-names :batman [_]
  {:princess "The Joker"
   :minister "Harley Quinn"
   :general "Two-Face"
   :wizard "Poison Ivy"
   :priestess "Robin"
   :knight "Bane"
   :clown "Catwoman"
   :soldier "Batman"})
(defmethod card-names :beast-academy [_]
  {:princess "Calamitous Clod"
   :minister "Professor Grok"
   :general "Sergeant Rote"
   :wizard "R & B"
   :priestess "Fiona"
   :knight "Kraken"
   :clown "Ms. Q"
   :soldier "Student"})
;; TODO: Lord of the Rings (variant)

;; Models
(defrecord Player
  [active
   position
   hand
   discard
   last-played
   deck-knowledge
   player-knowledge])

(defn make-deck-knowledge []
  "Instantiate a player's knowledge of other players"
  (frequencies court-deck))

(defn make-player-knowledge [total-players position]
  "Instantiate a player's knowledge of other players."
  (vec (repeat total-players (frequencies court-deck))))

(defn make-players
  ;; default to 4 players
  ([] (make-players 4))
  ([n]
   (when-not (< 1 n 5) (throw (Exception. "Only 2-4 players are allowed.")))
   (mapv #(->Player true % '() '() nil (make-deck-knowledge) (make-player-knowledge n %)) (range n))))

(defn make-state [total-players]
  (let [full-deck (shuffle court-deck)
        burned-card (first full-deck)
        deck (rest full-deck)]
    {:current-player 0
     :players (make-players total-players)
     :status :in-progress
     :deck deck
     :burned-card burned-card}))
