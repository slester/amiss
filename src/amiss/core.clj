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
  (flatten
    [:princess
     :minister
     :general
     (repeat 2 :wizard)
     (repeat 2 :priestess)
     (repeat 2 :knight)
     (repeat 2 :clown)
     (repeat 5 :soldier)]))

;; players map
;; * remove when they're out

;; sketch of player
;; * is-protected? -- has princess up
;; * hand: collection

;; UTILITIES ;;
(defn compare-cards [card-a card-b]
  "Compares two court cards: a=b => nil, a>b => true, a<b => false."
  (let [a (court card-a)
        b (court card-b)]
    (if (= a b) nil (> a b))))

;; GAME PLAY ;;
;; each of these should return a new game state
(defn start-game [num-players]
  "Start a new game! Shuffle, burn a card, then deal to the number of players."
  identity)

; TODO: return state
(defn burn-card [state]
  "Remove the top card from the deck."
  (drop 1 deck))

; TODO: return state
(defn draw-card [state player]
  "Player draws the top card from the deck."
  (let [deck (state :deck)
        hand (player :hand)]
    (conj hand (take 1 deck))))

; TODO: return state
(defn play-card [state player card]
  "Player plays the given card."
  identity)

; TODO: return state
(defn discard-card [state player card]
  "Discards a card."
  identity)

;; CARD POWERS ;;
; 8 - Princess
; TRIGGER
(defn is-princess? [card]
  "(Princess's Power) If you discard the princess, you're out of the round."
  (= :princess card))

; 7 - Minister
; TRIGGER
(defn triggers-minister? [player]
  "(Minister's Power) Check if a given player has the minister and 12 or more points in their hand. If so, that player is out of the round."
  identity)

; 6 - General
; ACTION: should return a state
(defn swap-hands [state player-a player-b]
  "(General's Power) Player A and player B swap hands."
  ; CAN add knowledge to someone's bank (if you know a card, swap, etc.)
  identity)

; 5 - Wizard
; ACTION: should return a state
(defn discard-hand [state target]
  "(Wizard's Power) Target player discards his hand and draws a new one."
  ; adds knowledge to everyone's bank
  identity)

; 4 - Priestess
; ACTION: should return a state
(defn barrier [state player]
  "(Priestess's Power) Player cannot be targeted."
  ; adds knowledge to everyone's bank (just priestess-1)
  identity)

; 3 - Knight
; ACTION: should return a state
(defn force-compare [player-a player-b]
  "(Knight's Power) Force two players to compare their hands. The lesser one is out of the round."
  ; adds knowledge to everyone's bank
  identity)

; 2 - Clown
; ACTION: should return a state
(defn reveal-hand [target-player show-to]
  "(Clown's Power) Player reveals his hand to another player."
  ; adds knowledge to show-to's bank
  identity)

; 1 - Soldier
; ACTION: should return a state
(defn guess-card [target guess]
  "(Soldier's Power) If the target player has the guessed card, that player is out of the round."
  ; adds knowledge to everyone's bank
  identity)
