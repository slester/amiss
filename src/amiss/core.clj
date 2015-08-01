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
(defn announce [s & args] (println (apply (partial format (str "[Game] " s)) args)))

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
  (let [players (state :players)
        out-player (first (mapcat (fn [i m] (if (and (< -1 (.indexOf (m :discard) :princess)) (m :active)) [i])) (range) players))]
    (cond-> state
      (not= nil out-player) (remove-player out-player))))

; 7 - Minister - PASSIVE
(defn check-minister [state]
  "(Minister's Power) Check if the current player has the minister and 12 or more points in their hand. If so, that player is out of the round."
  (let [current-id (state :current-player)
        p ((state :players) current-id)
        hand (p :hand)
        has-minister? (< -1 (.indexOf hand :minister))
        hand-12? (< 11 (reduce + 0 (map court hand)))]
    (if (and has-minister? hand-12?)
      (remove-player state current-id)
      state)))

; 6 - General - ACTION
(defn swap-hands [state target-id]
  "(General's Power) Player A and player B swap hands."
  ; CAN add knowledge to someone's bank (if you know a card, swap, etc.)
  (let [players (state :players)
        current-id (state :current-player)
        current (players current-id)
        target (players target-id)
        current-hand (current :hand)
        target-hand (target :hand)]
    (if (= current-id target-id)
      (do
        (announce "Player %d passes his turn." current-id)
        state)
      (do
        (announce "Player %d and player %d swap hands." (state :current-player) target-id)
        (-> state
            (assoc-in [:players current-id :hand] target-hand)
            (assoc-in [:players target-id :hand] current-hand))))))

; 5 - Wizard - ACTION - Can target self.
(defn discard-draw [state target-id]
  "(Wizard's Power) Target player discards his hand and draws a new one."
  (let [p ((state :players) target-id)
        card (first (p :hand))]
    (announce "Player %d targets player %d." (state :current-player) target-id)
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
(defn compare-hands [state target-id]
  "(Knight's Power) The player chooses a player, and they compare their hands. The lesser one is out of the round."
  (let [current-id (state :current-player)
        current ((state :players) current-id)
        target ((state :players) target-id)
        current-hand (first (current :hand))
        target-hand (first (target :hand))
        comparison (compare-cards current-hand target-hand)]
    (if (= current-id target-id)
      (do
        (announce "Player %d passes his turn." current-id)
        state)
      (do
        (announce "Player %d and player %d compare hands.." current-id target-id)
        (cond-> state
          ; a > b => true, a < b => false, tie = nil
          (= true comparison) (remove-player target-id)
          (= false comparison) (remove-player current-id))
        ))))

; 2 - Clown - ACTION
(defn reveal-hand [state target-id]
  "(Clown's Power) Player reveals his hand to another player."
  (let [current-id (state :current-player)]
    (announce "Player %d shows his hand to player %d." target-id (state :current-player))
    ; TODO reveal information
    (if (= current-id target-id)
      state
      state)))

; 1 - Soldier - ACTION
(defn guess-card [state target-id guess]
  "(Soldier's Power) If the target player has the guessed card, that player is out of the round."
  (let [p ((state :players) target-id)
        current-id (state :current-player)
        hand (first (p :hand))]
    (if (= current-id target-id)
      (do
        (announce "Player %d passes his turn." current-id)
        state)
      (do
        (announce "Player %d guesses that player %d is %s." current-id target-id guess)
        (cond-> state
          (= guess hand) (remove-player target-id))))))

(defn court-action [state played-card target guess]
  (condp = played-card
    :princess state
    :minister state
    :general (swap-hands state target)
    :wizard (discard-draw state target)
    :priestess state
    :knight (compare-hands state target)
    :clown (reveal-hand state target)
    :soldier (guess-card state target guess)))

;; GAME PLAY ;;
(defn burn-card [state]
  "Remove the top card from the deck and save it to the game state."
  (let [deck (state :deck)]
    (assoc state :burned-card (first deck) :deck (rest deck))))

(defn draw-card [state player-id]
  "Player draws the top card from the deck."
  (let [deck (state :deck)
        deck-empty? (= 0 (count deck))
        burned-card (state :burned-card)
        card (if deck-empty? burned-card (first (take 1 deck)))
        p ((state :players) player-id)
        hand (add-card (p :hand) card)
        new-deck (drop 1 deck)]
    (announce "Player %d draws a card." player-id)
    (omni "Player %d drew %s (hand is now %s)." player-id card (apply str hand))
    (cond-> state
      true (assoc-in [:players player-id :hand] hand)
      true (assoc :deck new-deck)
      deck-empty? (assoc :burned-card nil))))

(defn next-turn [state]
  "Moves to the next player's turn."
  (let [players (state :players)
        current-player-id (state :current-player)
        next-player-id (mod (inc current-player-id) (count players))
        next-player (players next-player-id)]
    (cond-> (assoc state :current-player next-player-id)
      true (assoc :phase 0)
      (not (next-player :active)) next-turn)))

(defn discard-card [state player-id card]
  "Discards a card."
  (let [p ((state :players) player-id)
        hand (p :hand)
        discard (p :discard)]
    ; Discarding the princess means you're out!
    (announce "Player %d discards %s." player-id card)
    (-> state
        (assoc-in [:players player-id :hand] (remove-card hand card))
        (assoc-in [:players player-id :discard] (add-card discard card)))))

(defn discard-hand [state player-id]
  "Discards the entire hand."
  (let [p ((state :players) player-id)
        hand (p :hand)
        discard (p :discard)]
    (announce "Player %d discards his entire hand: %s" player-id (apply str hand))
    (-> state
        (assoc-in [:players player-id :discard] (apply (partial conj discard) hand))
        (assoc-in [:players player-id :hand] '()))))

(defn play-card [state & {:keys [played-card target-id guess]}]
  "Current player plays the provided card, or a random one if none provided."
  (let [player-id (state :current-player)
        players (state :players)
        p (players player-id)
        valid-player-ids (mapcat (fn [i m] (if (and (not= (m :last-played) :priestess) (m :active)) [i])) (range) players)
        other-player-ids (remove-first valid-player-ids player-id)
        random-player-id (if (< 0 (count other-player-ids)) (rand-nth other-player-ids) player-id)
        target-player (if (nil? target-id) random-player-id target-id)
        non-soldiers (filter #(not= :soldier %) (state :deck))
        guessed-card (if (nil? guess) (if (< 0 (count non-soldiers)) (rand-nth non-soldiers) :princess) guess)
        card (if (nil? played-card) (rand-nth (p :hand)) played-card)]

    (-> state
        (assoc-in [:players player-id :last-played] card)
        (discard-card player-id card)
        (court-action card target-player guessed-card))))

(defn remove-player [state player-id]
  "A player is removed from the round."
  (announce "Player %d is out of the round." player-id)
  (-> state
      (assoc-in [:players player-id :active] false)
      (discard-hand player-id)))

(defn final-summary [state]
  "Calculates & congratulates the winners!"
  (let [winners (mapcat (fn [i m] (if (true? (m :active)) [i])) (range) (state :players))]
    (when (< 1 (count winners))
      (do
        (announce "The following players have tied: %s" (clojure.string/join ", " winners))
        ;; TODO break ties
        ))
    (announce "The winner is player %d!" (first winners))
    state))

(defn check-end-game [state]
  "Checks to see if the game should end."
  (omni "Checking if we should end the game... %s" (game-over? state))
  (if (game-over? state)
    (-> (assoc state :status :over) final-summary)
    state))

(defn next-phase [state]
  "Moves on to the next phase of a turn."
  (let [phase (mod (inc (state :phase)) 3)]
    (omni "Phase %d" phase)
    (assoc state :phase phase)))

; TODO - the final message doesn't play if the game ends on phase 0/2 -- why?
; game will end if you take an extra step, so everything's working, must be timing?
(defn play [state]
  "Perform the next step in a player's turn."
  (let [phase (state :phase)
        current (state :current-player)
        _ (println (check-end-game state))
        updated-state (-> state check-minister check-princess check-end-game)
        players (updated-state :players)
        current-active? ((players current) :active)
        over? (game-over? updated-state)]
    (omni-state updated-state)
    (cond
      ;; If the game's over, there's nothing to be done!
      over? updated-state
      ;; If the current player is out of the round, move on to the next turn.
      (not current-active?) (-> updated-state next-turn)
      ;; Otherwise, perform the proper action for the phase
      (= phase 0) (-> updated-state (draw-card current) next-phase)
      (= phase 1) (-> updated-state play-card next-phase)
      (= phase 2) (-> updated-state next-turn))))

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
    (announce "A new game with %d players begins." num-players)
    (-> state
        burn-card
        ((partial reduce draw-card) (range num-players))
        (assoc :status :playing))))
