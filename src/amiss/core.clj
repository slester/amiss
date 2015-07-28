(ns amiss.core)

(def court
  {:princess  8
   :minister  7
   :general   6
   :wizard    5
   :priestess 4
   :knight    3
   :clown     2
   :soldier   1})

(def deck
  {:princess  1
   :minister  1
   :general   1
   :wizard    2
   :priestess 2
   :knight    2
   :clown     2
   :soldier   5})

;; players map
;; * remove when they're out

;; sketch of player
;; * is-protected? -- has princess up
;; * hand: collection

;; UTILITIES ;;
(defn start-game [num-players]
  "Start a new game! Shuffle, burn a card, then deal to the number of players."
  identity)

(defn burn-card [deck]
  "Remove the top card from the deck."
  (drop 1 deck))

(defn compare-cards [card-a card-b]
  "Compares two court cards: a=b => nil, a>b => true, a<b => false."
  (let [a (court card-a)
        b (court card-b)]
    (if (= a b) nil (> a b))))

(defn shuffle-deck [deck]
  "Shuffles the deck."
  (shuffle deck))

; TODO: return deck or player here?
(defn draw-card [player deck]
  "Player draws the top card from the deck."
  (take 1 deck))

(defn play-card [player card]
  "Player plays the given card."
  identity)

(defn discard-card [player card]
  "Discards a card."
  identity)

;; CARD POWERS ;;
; 8 - Princess
(defn is-princess? [card]
  "(Princess's Power) If you discard the princess, you're out of the round."
  (= :princess card))

; 7 - Minister
(defn triggers-minister? [player]
  "(Minister's Power) Check if a given player has the minister and 12 or more points in their hand. If so, that player is out of the round."
  identity)

; 6 - General
(defn swap-hands [player-a player-b]
  "(General's Power) Player A and player B swap hands."
  identity)

; 5 - Wizard
(defn discard-hand [player]
  "(Wizard's Power) Target player discards his hand and draws a new one."
  identity)

; 4 - Priestess
(defn barrier [player]
  "(Priestess's Power) Player cannot be targeted."
  identity)

; 3 - Knight
(defn force-compare [player-a player-b]
  "(Knight's Power) Force two players to compare their hands. The lesser one is out of the round."
  identity)

; 2 - Clown
(defn reveal-hand [target-player show-to]
  "(Clown's Power) Player reveals his hand to another player."
  identity)

; 1 - Soldier
(defn guess-card [target guess]
  "(Soldier's Power) If the target player has the guessed card, that player is out of the round."
  identity)
