(ns amiss.core
  (:require [clojure.pprint :as p :refer [pprint]]))

(def court
  {:princess  8
   :minister  7
   :general   6
   :wizard    5
   :priestess 4
   :knight    3
   :clown     2
   :soldier   1})

(defonce full-deck
  (flatten
    [:princess
     :minister
     :general
     (repeat 2 :wizard)
     (repeat 2 :priestess)
     (repeat 2 :knight)
     (repeat 2 :clown)
     (repeat 5 :soldier)]))

(defn player []
  "A player object containing a hand, last card played, knowledge of other players, and knowledge of the deck."
  {:active true
   :hand '()
   :last-card nil
   :deck-knowledge {}
   :player-knowledge []})

;; rule sets
;; - original
;; - tempest

;; players map
;; * remove when they're out

;; sketch of player
;; * last card
;; * hand: collection
;; * knowledge -- can start every player with equal % chance of every card in deck-knowledge, then adjust

;; UTILITIES ;;
(defonce is-dev? true)
(defn omni [s & args] (if is-dev? (println (apply (partial format (str "[Omniscience] " s)) args)) identity))

; Lets you keep track of the state when dev mode is on.
; You can put this inside a -> macro!
(defn omni-state [state]
  (when is-dev?
    (print "[Omniscience] Current game state: ")
    (p/pprint state))
  state)

(defn compare-cards [card-a card-b]
  "Compares two court cards: a=b => nil, a>b => true, a<b => false."
  (let [a (court card-a)
        b (court card-b)]
    (if (= a b) nil (> a b))))

;; KNOWLEDGE ;;
(defn update-deck-knowledge [state]
  ; take into account previous player's played card
  )

;; GAME PLAY ;;
(defn burn-card [state]
  "Remove the top card from the deck and save it to the game state."
  (let [deck (state :deck)]
    (-> state
        (assoc-in [:burned-card] (first deck))
        (assoc-in [:deck] (rest deck)))))

(defn draw-card [state player-id]
  "Player draws the top card from the deck."
  (let [deck (state :deck)
        cards-in-deck (count deck)
        burned-card (state :burned-card)
        player (nth (state :players) player-id)
        ; If there aren't any cards left, take the burned card.
        ; TODO: remove the burned card. what if it tries to draw nil?
        card (if (> 0 cards-in-deck) (first (take 1 deck)) burned-card)
        hand (conj (player :hand) card)
        new-deck (drop 1 deck)]
    (omni "Player %d just drew %s (hand is now %s)." player-id card (apply str hand))
    (-> state
        (assoc-in [:players player-id :hand] hand)
        (assoc-in [:deck] new-deck))))

(defn remove-player [state player-id]
  "A player is removed from the round."
  (let [player (nth (state :players) player-id)]
    (assoc-in state [:players player-id :active] false)))

(defn check-for-win [state]
  "Check the board for winning state, i.e. only one person left active."
  (let [players (state :players)]
    (= 1 (count (filter #(= true (% :active)) players)))))

(defn next-turn [state]
  "Moves to the next player's turn."
  (let [players (state :players)
        current-player-id (state :current-player)
        next-player-id (mod (count players) (inc current-player-id))
        next-player (nth players next-player-id)]
    (cond-> (assoc-in state [:current-player] next-player-id)
      (= false (next-player :active)) next-turn)))

; TODO: return state
(defn play-card [state player-id card]
  "Player plays the given card."
  identity)

; TODO: return state
(defn discard-card [state player-id card]
  "Discards a card."
  (let [player (nth (state :players) player-id)
        hand (player :hand)]
    ; Discarding the princess means you're out!
    (cond-> (assoc-in state [:players player-id :hand] (disj hand card))
      (= card :princess) (remove-player player-id))))

(defn start-game [num-players]
  {:pre [(< 1 num-players)
         (> 5 num-players)]}
  "Start a new game of 2-4 players! Shuffle, burn a card, then deal to the number of players."
  (let [deck (shuffle full-deck)
        state {:current-player 0
               :players (vec (take num-players (repeatedly player)))
               :deck deck}
        players (state :players)]
    (omni "A new game with %d players begins." num-players)
    (-> state
        burn-card
        ((partial reduce draw-card) (range num-players))
        omni-state)))

;; CARD POWERS ;;
; 8 - Princess
; TRIGGER
(defn is-princess? [card]
  "(Princess's Power) If you discard the princess, you're out of the round."
  (= :princess card))

; 7 - Minister
; TRIGGER
(defn triggers-minister? [state player-id]
  "(Minister's Power) Check if a given player has the minister and 12 or more points in their hand. If so, that player is out of the round."
  (let [p (nth (state :players) player-id)
        hand (player :hand)]
    (< 11 (reduce + 0 (map court hand)))))

; 6 - General
; ACTION: should return a state
(defn swap-hands [state a-id b-id]
  "(General's Power) Player A and player B swap hands."
  ; CAN add knowledge to someone's bank (if you know a card, swap, etc.)
  identity)

; 5 - Wizard
; ACTION: should return a state
(defn discard-hand [state target-id]
  "(Wizard's Power) Target player discards his hand and draws a new one."
  (let [p (nth (state :players) target-id)
        card (first (p :hand))]
  (-> state
      (discard-card target-id card)
      (draw-card target-id))))

; 4 - Priestess
; TRIGGER
(defn has-barrier? [state player-id]
  "(Priestess's Power) Player cannot be targeted."
  (let [p (nth (state :players) player-id)
        last-card (p :last-card)]
    (= last-card :priestess)))

; 3 - Knight
; ACTION: should return a state
(defn force-compare [state a-id b-id]
  "(Knight's Power) Force two players to compare their hands. The lesser one is out of the round."
  (let [a (nth (state :players) a-id)
        b (nth (state :players) b-id)
        a-hand (nth (a :hand) 0)
        b-hand (nth (b :hand) 0)
        comparison (compare-cards a-hand b-hand)]
    (cond-> state
      (= true comparison) (remove-player b-id)
      (= false comparison) (remove-player a-id))))

; 2 - Clown
; ACTION: should return a state
(defn reveal-hand [state target-id show-to]
  "(Clown's Power) Player reveals his hand to another player."
  identity)

; 1 - Soldier
; ACTION: should return a state
(defn guess-card [state target-id guess]
  "(Soldier's Power) If the target player has the guessed card, that player is out of the round."
  (let [p (nth (state :players) target-id)
        hand (nth (p :hand) 0)]
    (cond-> state
      (= guess hand) (remove-player target-id))))
