(ns amiss.core
  (:require [clojure.pprint :as p :refer [pprint]]))

(defonce court
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
   :discard '()
   :last-played nil
   :deck-knowledge {}
   :player-knowledge []})

;; rule sets
;; - original (Japanese)
;; -- names
;; -- can tie at end, no counting discards
;; -- 7 card => lose if 12+
;; -- can target priestess with no effect
;; - Tempest
;; -- names
;; -- ties are broken by summing discards; can still result in a tie afterward
;; -- 7 card => discard 7 card if 5 or 6
;; -- cannot target handmaids; must choose self if possible, otherwise no effect

;; turn flow
;; 1. is the game over? if so, stop!
;; 2. current player draws a card
;;   - check for minister
;; 3. play a card
;;  - check for playing princess
;;  - do action for card
;;  -- kick people out
;; 4. go to next active player

;; players map
;; * remove when they're out

;; sketch of player
;; * last card
;; * hand: collection
;; * knowledge -- can start every player with equal % chance of every card in deck-knowledge, then adjust

;; DEBUG ;;
(defonce is-dev? true)
(defn omni [s & args] (if is-dev? (println (apply (partial format (str "[Omniscience] " s)) args)) identity))

; Lets you keep track of the state when dev mode is on.
; You can put this inside a -> macro!
(defn omni-state [state]
  (when is-dev?
    (print "[Omniscience] Current game state: ")
    ;; (pprint state))
    (println state))
  state)

;; UTILITIES ;;
(defn compare-cards [card-a card-b]
  "Compares two court cards: a=b => nil, a>b => true, a<b => false."
  (let [a (court card-a)
        b (court card-b)]
    ((comp {-1 false 0 nil 1 true} compare) a b)))

(defn get-active [state]
  "Determine the players still in the running."
  (let [players (state :players)]
        (filter #(= true (% :active)) players)))

(defn game-over? [state]
  "Check the board for winning state, i.e. only one person left active or the deck is empty."
  (let [players (state :players)]
    (or (= 1 (count (get-active state)))
        (= 0 (count (state :deck))))))


;; KNOWLEDGE ;;
(defn update-deck-knowledge [state]
  ; take into account previous player's played card
  )

;; GAME PLAY ;;
(defn burn-card [state]
  "Remove the top card from the deck and save it to the game state."
  (let [deck (state :deck)]
    (assoc state :burned-card (first deck) :deck (rest deck))))

(defn draw-card [state player-id]
  "Player draws the top card from the deck."
  (let [deck (state :deck)
        over? (game-over? state)
        ; If not the current player, the player is being forced to draw this card.
        ; This may mean they have to draw a burned card if the game's over.
        force-draw? (not= player-id (state :current-player))
        burned-card (state :burned-card)
        ; Draw a card, add the card to the player's hand, return the new deck.
        player (nth (state :players) player-id)
        card (if (and over? force-draw?) burned-card (first (take 1 deck)))
        hand (conj (player :hand) card)
        new-deck (drop 1 deck)
        hand-updated? (or (not over?) force-draw?)]
    (when hand-updated? (omni "Player %d just drew %s (hand is now %s)." player-id card (apply str hand)))
    (cond-> state
      hand-updated? (assoc-in [:players player-id :hand] hand)
      (not over?) (assoc-in [:deck] new-deck)
      (and over? force-draw?) (assoc-in [:burned-card] nil) ; Remove the burned card if we drew it.
      over? (assoc-in [:status] :over))))

(defn remove-player [state player-id]
  "A player is removed from the round."
  (let [player (nth (state :players) player-id)]
    (assoc-in state [:players player-id :active] false)))

(defn next-turn [state]
  "Moves to the next player's turn."
  (let [players (state :players)
        current-player-id (state :current-player)
        next-player-id (mod (inc current-player-id) (count players))
        next-player (nth players next-player-id)]
    (cond-> (assoc-in state [:current-player] next-player-id)
      (= false (next-player :active)) next-turn)))

; TODO: return state
(defn play-card [state player-id card]
  "Player plays the given card."
  ;; call power of card chosen
  ;; call discard-card
  identity)

(defn discard-card [state player-id card]
  "Discards a card."
  (let [player (nth (state :players) player-id)
        hand (player :hand)
        discard (player :discard)]
    ; Discarding the princess means you're out!
    (cond-> (assoc-in state [:players player-id :hand] (disj hand card))
      true (assoc-in state [:players player-id :discard] (conj discard card))
      (= card :princess) (remove-player player-id))))

(defn start-game [num-players]
  {:pre [(< 1 num-players 5)]}
  "Start a new game of 2-4 players! Shuffle, burn a card, then deal to the number of players."
  (let [deck (shuffle full-deck)
        state {:status :begin
               :current-player 0
               :players (vec (take num-players (repeatedly player)))
               :deck deck}
        players (state :players)]
    (omni "A new game with %d players begins." num-players)
    (-> state
        burn-card
        ((partial reduce draw-card) (range num-players))
        omni-state
        (assoc-in [:status] :playing))))

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
  (let [players (state :players)
        a (:players a-id)
        b (:players b-id)
        a-hand (a :hand)
        b-hand (b :hand)]
    (-> state
        (assoc-in [:players a-id :hand] b-hand)
        (assoc-in [:players b-id :hand] a-hand))))

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
        last-played (p :last-played)]
    (= last-played :priestess)))

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
(defn reveal-hand [state show-to target-id]
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

(defn court-action [state played-card a-id b-id target-card]
  (let action-map {:princess (identity state)
                   :minister (identity state)
                   :general (identity state)
                   :wizard (identity state)
                   :priestess (identity state)
                   :knight (identity state)
                   :clown (identity state)
                   :soldier (identity state)})
  (action-map card))
