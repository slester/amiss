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
    (print "\n[Omniscience] Current game state: " state "\n\n")
    ;; (pprint state))
    ;; (println state "\n")
    state))

;; UTILITIES ;;
(defn remove-first [coll item] (let [sp (split-with (partial not= item) coll)] (concat (first sp) (rest (second sp)))))
(defn add-card [coll card] (conj coll card))
(defn remove-card [coll card] (remove-first coll card))

(defn compare-cards [card-a card-b]
  "Compares two court cards: a=b => nil, a>b => true, a<b => false."
  (let [a (court card-a)
        b (court card-b)]
    ((comp {-1 false 0 nil 1 true} compare) a b)))

(defn get-active [state]
  "Determine the players still playing."
  (let [players (state :players)]
    (filter :active players)))

(defn game-over? [state]
  "Check the board for winning state, i.e. only one person left active or the deck is empty."
  (let [players (state :players)]
    (or (= 1 (count (get-active state)))
        (= 0 (count (state :deck))))))

;; KNOWLEDGE ;;
(defn update-deck-knowledge [state]
  ; take into account previous player's played card
  )

;; GAME PLAY DECLARATIONS ;;
;; These are used in card powers.
(declare remove-player)
(declare draw-card)
(declare discard-card)


;; CARD POWERS ;;
; 8 - Princess - PASSIVE
(defn check-princess [state]
  "(Princess's Power) If you discard the princess, you're out of the round."
  (let [players (state :players)]
    ; TODO: check all active players' discards & kick out any with princess
    state
    ))

; 7 - Minister - PASSIVE
(defn check-minister [state]
  "(Minister's Power) Check if the current player has the minister and 12 or more points in their hand. If so, that player is out of the round."
  (let [current-id (state :current-player)
        p ((state :players) current-id)
        hand (p :hand)
        has-minister? (< -1 (.indexOf hand :minister))
        hand-12? (< 11 (reduce + 0 (map court hand))) ]
    (if (and has-minister? hand-12?)
      (remove-player state current-id)
      state)))

; 6 - General - ACTION
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

; 5 - Wizard - ACTION
(defn discard-draw [state target-id]
  "(Wizard's Power) Target player discards his hand and draws a new one."
  (let [p ((state :players) target-id)
        card (first (p :hand))]
    (-> state
        (discard-card target-id card)
        (draw-card target-id))))

; 4 - Priestess - PASSIVE
(defn has-barrier? [state player-id]
  "(Priestess's Power) Player cannot be targeted."
  (let [p ((state :players) player-id)
        last-played (p :last-played)]
    (= last-played :priestess)))

; 3 - Knight - ACTION
(defn force-compare [state a-id b-id]
  "(Knight's Power) Force two players to compare their hands. The lesser one is out of the round."
  (let [a ((state :players) a-id)
        b ((state :players) b-id)
        a-hand ((a :hand) 0)
        b-hand ((b :hand) 0)
        comparison (compare-cards a-hand b-hand)]
    (cond-> state
      (= true comparison) (remove-player b-id)
      (= false comparison) (remove-player a-id))))

; 2 - Clown - ACTION
(defn reveal-hand [state show-to target-id]
  "(Clown's Power) Player reveals his hand to another player."
  identity)

; 1 - Soldier - ACTION
(defn guess-card [state target-id guess]
  "(Soldier's Power) If the target player has the guessed card, that player is out of the round."
  (let [p ((state :players) target-id)
        hand ((p :hand) 0)]
    (cond-> state
      (= guess hand) (remove-player target-id))))

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
        p ((state :players) player-id)
        card (if (and over? force-draw?) burned-card (first (take 1 deck)))
        hand (add-card (p :hand) card)
        new-deck (drop 1 deck)
        hand-updated? (or (not over?) force-draw?)]
    (when hand-updated? (omni "Player %d draws %s (hand is now %s)." player-id card (apply str hand)))
    (cond-> state
      hand-updated? (assoc-in [:players player-id :hand] hand)
      ;; hand-updated? (check-minister player-id) ; This should be moved into the game loop, I think
      (not over?) (assoc-in [:deck] new-deck)
      (and over? force-draw?) (assoc-in [:burned-card] nil))))

; TODO: randomly fails test, nil pointer exception
(defn next-turn [state]
  "Moves to the next player's turn."
  (let [players (state :players)
        current-player-id (state :current-player)
        next-player-id (mod (inc current-player-id) (count players))
        next-player (players next-player-id)]
    (cond-> (assoc-in state [:current-player] next-player-id)
      (not (next-player :active)) next-turn)))

(defn discard-card [state player-id card]
  "Discards a card."
  (let [p ((state :players) player-id)
        hand (p :hand)
        discard (p :discard)]
    ; Discarding the princess means you're out!
    (-> state
        (assoc-in [:players player-id :hand] (remove-card hand card))
        (assoc-in [:players player-id :discard] (add-card discard card))
        ;; (check-princess player-id) ; should be checked during the game loop
        )))

(defn discard-hand [state player-id]
  "Discards the entire hand."
  (let [p ((state :players) player-id)
        hand (p :hand)
        discard (p :discard)]
    (omni "Player %d discards his entire hand (%s)." player-id hand)
    (-> state
        (assoc-in [:players player-id :discard] (conj discard hand))
        (assoc-in [:players player-id :hand] '()))))

(defn play-card [state & chosen-card]
  "Current player plays the provided card, or a random one if none provided."
  (let [player-id (state :current-player)
        p ((state :players) player-id)
        card (if (nil? chosen-card) (rand-nth (p :hand)) chosen-card)]
    (omni "Player %d discards %s." player-id card)
    (-> state
        ;;TODO: call power of card chosen
        (assoc-in [:players player-id :last-played] card)
        (discard-card player-id card))))

(defn remove-player [state player-id]
  "A player is removed from the round."
  (omni "Player %d is out of the round." player-id)
  (-> state
      (assoc-in [:players player-id :active] false)
      (discard-hand player-id)))

(defn final-summary [state]
  "Calculates & congratulates the winners!"
  (omni "Here are the winners: %s" (get-active state))
  state
  )

(defn end-game [state]
  "Checks to see if the game should end."
  (if (game-over? state)
    (-> (assoc state :status :over) final-summary)
    state))

; TODO
; - rounds should 'short-circuit':
; ** state can have a 'step' -- if a player is kicked out, go to the 'end' step that tests for game end, otherwise, go to step x+1
; ** end step checks for game over -- if not, call next-turn and go to step 0
;; <justin_smith> slester: you could break run-turn into the individual operations that could potentially trigger an end game, and only run one of those per iteration
;; <justin_smith> all it takes is passing a value representing which sub-item of a turn you are in
;; <justin_smith> and dispatching on that of course
;; <justin_smith> essentially an immutable state machine

; 0 - draw-card
; 1 - play-card
; 2 - next-turn
(defn next-phase [state]
  "Moves on to the next phase of a turn."
  (let [phase (inc (state :phase))]
  (assoc state :phase (mod phase 3))))

;; before every phase:
;; check if current is still in the game
;; check if game is over
(defn play [state]
  "Perform the next step in a player's turn."
  (let [phase (state :phase)
        current (state :current-player)
        updated-state (-> state check-minister check-princess end-game)
        players (updated-state :players)
        current-active? ((players current) :active)
        over? (game-over? updated-state)]
    (cond
      ;; If the game's over, there's nothing to be done!
      over? updated-state
      ;; If the current player is out of the round, move on to the next turn.
      (not current-active?) next-turn
      ;; Otherwise, perform the proper action for the phase
      (= phase 0) (-> updated-state (draw-card current) next-phase)
      (= phase 1) (-> updated-state play-card next-phase)
      (= phase 2) (-> updated-state next-turn next-phase)
      )
    )
  )

(defn start-game [num-players]
  {:pre [(< 1 num-players 5)]}
  "Start a new game of 2-4 players! Shuffle, burn a card, then deal to the number of players."
  (let [deck (shuffle full-deck)
        state {:status :begin
               :phase 0
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

(defn court-action [state played-card a-id b-id target-card]
  (let [action-map {:princess (identity state)
                    :minister (identity state)
                    :general (identity state)
                    :wizard (identity state)
                    :priestess (identity state)
                    :knight (identity state)
                    :clown (identity state)
                    :soldier (identity state)}]))
