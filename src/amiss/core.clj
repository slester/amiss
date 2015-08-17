(ns amiss.core)

;; overarching TODO
;; -- naming conventions: player or current?

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

;; DEBUG ;;
(defonce is-dev? true)
(defn omni [s & args] (if is-dev? (println (apply (partial format (str "[Omniscience] " s)) args)) identity))
(defn announce [s & args] (println (apply (partial format (str "[Game] " s)) args)))
(defn omni-state [state] (when is-dev?  (print "\n[Omniscience] Current game state: " state "\n\n") state))

;; UTILITIES ;;
(defn remove-first [coll item] (let [sp (split-with (partial not= item) coll)] (concat (first sp) (rest (second sp)))))
(defn add-card [coll card] (conj coll card))
(defn remove-card [coll card] (remove-first coll card))
(defn seq-contains? [coll card] (boolean (some #(= % card) coll)))
(defn delete-element [v pos] (vec (concat (subvec v 0 pos) (subvec v (inc pos)))))

(defn compare-cards [card-a card-b]
  "Compares two court cards: a=b => nil, a>b => true, a<b => false."
  (let [a (court card-a)
        b (court card-b)]
    ((comp {-1 false 0 nil 1 true} compare) a b)))

(defn cards-higher-than [card]
  "Get a list of all cards with a greater value than a certain card."
  (let [card-value (court card)]
    (filter #(< card-value (court %)) (keys court))))

(defn get-active [state]
  "Determine the players still playing."
  (let [players (state :players)]
    (keep #(if ((players %) :active) %) (range (count players)))))

(defn all-but-player [state player-id]
  "Return all player IDs except the indicated player."
  (let [players (state :players)]
    (remove #{player-id} (vec (get-active state)))))

(defn game-over? [state]
  "Check the board for winning state, i.e. only one person left active or the deck is empty after all cards have been played."
  (let [players (state :players)
        current-id (state :current-player)
        current (players current-id)]
    (or (= 1 (count (get-active state)))
        (and (= 0 (count (state :deck))) (= 1 (count (current :hand)))))))

;; GAME PLAY DECLARATIONS ;;
;; These are used in card powers.
(declare remove-player)
(declare draw-card)
(declare discard-card)
(declare remove-deck-knowledge)
(declare add-player-knowledge)
(declare remove-player-knowledge)
(declare swap-player-knowledge)
(declare pick-card-to-play)

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
            (assoc-in [:players target-id :hand] current-hand)
            (swap-player-knowledge current-id target-id)
            (add-player-knowledge current-id target-id current-hand)
            (add-player-knowledge target-id current-id target-hand))))))

; 5 - Wizard - ACTION - Can target self.
(defn discard-draw [state target-id]
  "(Wizard's Power) Target player discards his hand and draws a new one."
  (let [p ((state :players) target-id)
        card (first (p :hand))]
    (announce "Player %d targets player %d." (state :current-player) target-id)
    (cond-> state
      true (discard-card target-id card)
      (not= card :princess) (draw-card target-id))))

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
        players (state :players)
        current (players current-id)
        target (players target-id)
        current-hand (first (current :hand))
        target-hand (first (target :hand))
        comparison (compare-cards current-hand target-hand)]
    (if (= current-id target-id)
      (do
        (announce "Player %d passes his turn." current-id)
        state)
      (do
        (announce "Player %d and player %d compare hands.." current-id target-id)
        (cond
          ;; a > b => true, a < b => false, tie = nil
          ;; We know the player had a higher card than the target, everyone gets that information.
          (= true comparison) (-> state
                                  (remove-player target-id)
                                  ((partial reduce (fn [s v] (add-player-knowledge s current-id v (cards-higher-than target-hand)))) (all-but-player state current-id)))
          ;; We know the target had a higher card than the player; everyone gets this information.
          (= false comparison) (-> state
                                   (remove-player current-id)
                                   ((partial reduce (fn [s v] (add-player-knowledge s target-id v (cards-higher-than current-hand)))) (all-but-player state target-id)))
          ;; If they tie, we know they're the same! TODO as this could get really difficult
          :else (-> state))))))

; 2 - Clown - ACTION
(defn reveal-hand [state target-id]
  "(Clown's Power) Player reveals his hand to another player."
  (let [current-id (state :current-player)
        players (state :players)
        target (players target-id)
        target-hand (target :hand)]
    (if (= current-id target-id)
      ;; If the player has to choose herself, nothing happens.
      (do
        (announce "Player %d passes his turn." current-id)
        state)
      ;; Otherwise, the current player gets extra information!
      (do
        (announce "Player %d targets player %d and looks at his hand." current-id target-id)
        (add-player-knowledge state current-id target-id target-hand)))))

; 1 - Soldier - ACTION
(defn guess-card [state target-id guess]
  "(Soldier's Power) If the target player has the guessed card, that player is out of the round."
  (let [players (state :players)
        p (players target-id)
        current-id (state :current-player)
        hand (first (p :hand))]
    (if (= current-id target-id)
      (do
        (announce "Player %d passes his turn." current-id)
        state)
      (do
        (announce "Player %d guesses that player %d is %s." current-id target-id guess)
        (if (= guess hand)
          ;; Everyone learns something about the deck when the player discards in remove-player.
          (-> state
              (remove-player target-id))
          ;; Otherwise, we learn that the person could be anyone in the deck MINUS the guess.
          (do (announce "Player %d is not a %s!" target-id guess)
              (-> state
                  ((partial reduce (fn [s v] (remove-player-knowledge s v target-id (list guess)))) (all-but-player state target-id)))))))))

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
      true (remove-deck-knowledge player-id card)
      true ((partial reduce (fn [s v] (remove-player-knowledge s player-id v (list card)))) (all-but-player state player-id))
      deck-empty? (assoc :burned-card nil))))

(defn next-turn [state]
  "Moves to the next player's turn."
  (let [players (state :players)
        current-id (state :current-player)
        next-id (mod (inc current-id) (count players))
        next-player (players next-id)]
    (cond-> (assoc state :current-player next-id)
      true (assoc :phase 0)
      (not (next-player :active)) next-turn)))

(defn discard-card [state player-id card]
  "Discards a card."
  (let [players (state :players)
        p (players player-id)
        hand (p :hand)
        discard (p :discard)]
    (announce "Player %d discards %s." player-id card)
    (-> state
        (assoc-in [:players player-id :hand] (remove-card hand card))
        (assoc-in [:players player-id :discard] (add-card discard card))
        ((partial reduce (fn [s v] (remove-deck-knowledge s v card))) (all-but-player state player-id))
        ;; Remove from everyone's knowledge of every player
        ((partial reduce (fn [s v]
                           (reduce (fn [t w] (remove-player-knowledge t v w (list card))) s (all-but-player state player-id)))) (all-but-player state player-id)))))

(defn discard-hand [state player-id]
  "Discards the entire hand."
  (let [p ((state :players) player-id)
        hand (p :hand)
        discard (p :discard)]
    (reduce (fn [s card] (discard-card s player-id card)) state hand)))

(defn random-card [state]
  "Pick a random card from your hand to play (except the princess)."
  (let [players (state :players)
        current-id (state :current-player)
        current-player (players current-id)
        hand (current-player :hand)
        target-id (rand-nth (all-but-player state current-id))
        target-knowledge ((current-player :player-knowledge) target-id)
        guess (rand-nth (keys (filter #(< 0 (second %)) target-knowledge)))]
    (if (some #(= :princess %) hand)
      (list (first (filter #(not= :princess %) hand)) target-id guess)
      (list (rand-nth hand) target-id guess))))

(defn play-card [state & {:keys [played-card target-id guess]}]
  "Current player plays the provided card, or a random one if none provided."
  (let [player-id (state :current-player)
        players (state :players)
        p (players player-id)
        play-tuple (pick-card-to-play state)
        card (nth play-tuple 0)
        target-id (nth play-tuple 1)
        guess (nth play-tuple 2)]
    (-> state
        (assoc-in [:players player-id :last-played] card)
        (discard-card player-id card)
        (court-action card target-id guess))))

(defn remove-player [state player-id]
  "A player is removed from the round."
  (announce "Player %d is out of the round." player-id)
  (-> state
      (discard-hand player-id)
      ((partial reduce (fn [s v] (assoc-in s [:players v :player-knowledge player-id] {}))) (all-but-player state player-id))
      (assoc-in [:players player-id :active] false)))

(defn final-summary [state]
  "Calculates & congratulates the winners!"
  (let [players (state :players)
        finalists (mapcat (fn [i m] (if (m :active) [i])) (range) players)
        finalists-hands (map #(court (first ((players %) :hand))) finalists)
        highest-value (first (reverse (sort finalists-hands)))
        _ (println finalists-hands highest-value)
        finalists-combined (zipmap finalists finalists-hands)
        winners (keys (filter #(= highest-value (second %)) finalists-combined))]
    (if (< 1 (count winners))
      (do
        ;; TODO break ties if Tempest
        (announce "The following players have tied: %s" (clojure.string/join ", " winners))
        state)
      (do
        (announce "The winner is player %d!" (first winners))
        state))))

(defn check-end-game [state]
  "Checks to see if the game should end."
  (if (game-over? state)
    (-> (assoc state :status :over))
    state))

(defn next-phase [state]
  "Moves on to the next phase of a turn."
  (let [phase (mod (inc (state :phase)) 3)]
    (assoc state :phase phase)))

(defn play [state]
  "Perform the next step in a player's turn. Iterating over this function drives game play."
  (let [phase (state :phase)
        current (state :current-player)
        updated-state (-> state check-minister check-princess check-end-game)
        players (updated-state :players)
        current-active? ((players current) :active)
        over? (game-over? updated-state)]
    (cond
      ;; If the game's over, there's nothing to be done!
      over? updated-state
      ;; If the current player is out of the round, move on to the next turn.
      (not current-active?) (-> updated-state next-turn)
      ;; Otherwise, perform the proper action for the phase
      (= phase 0) (-> updated-state (draw-card current) next-phase)
      (= phase 1) (-> updated-state play-card next-phase)
      (= phase 2) (-> updated-state next-turn))))

;;; KNOWLEDGE ;;;
(defn initialize-knowledge [state]
  (let [players (state :players)
        num-players (count players)]
    (-> state
        (assoc :players (vec (map
                               ;; Players start out having no knowledge of the deck or of other players.
                               #(assoc %
                                       :deck-knowledge (frequencies full-deck)
                                       :player-knowledge (vec (repeat num-players (frequencies full-deck))))
                               players))))))

(defn remove-deck-knowledge [state player-id card]
  "Remove a known card from a player's deck knowledge."
  (let [players (state :players)
        p (players player-id)
        deck-knowledge (p :deck-knowledge)]
    ;; (omni "Removing %s from player %d's deck knowledge." card player-id)
    (-> state
        (assoc-in [:players player-id :deck-knowledge card] (dec (deck-knowledge card))))))

;; Given what you know about a player, what are the probabilities of what is in her hand?
(defn card-probabilities [player-knowledge]
  "Calculate the probabilities of what is in a player's hand, given knowledge about it."
  ;; sum up all cards in hand, divide by total
  (let [total (reduce (fn [sum card] (+ sum (player-knowledge card))) 0 (keys player-knowledge))]
    (into {} (for [[k v] player-knowledge] [k (if (= 0 total) 0 (/ v total))]))))

;; Should the player play a soldier?
(defn play-soldier? [state]
  ;; Get all the other players' probabilities, sort by highest probability, if >0.50
  (let [current-id (state :current-player)
        players (state :players)
        p (players current-id)
        hand (p :hand)
        all-player-knowledge (p :player-knowledge)
        all-probabilities (vec (map card-probabilities all-player-knowledge))]
    (if (seq-contains? hand :soldier)
      (first (filter some? (map (fn [target] (let [probs (dissoc (nth all-probabilities target) :soldier)
                                                   top-prob-map (into (sorted-map-by (fn [k1 k2] (>= (probs k1) (probs k2)))) probs)]
                                               (omni "in reduce - %s" (list :soldier target (key (first top-prob-map))))
                                               (if (< 0.5 (val (first top-prob-map)))
                                                 (list :soldier target (key (first top-prob-map))))))
                                (all-but-player state current-id)))))))

(defn play-clown? [state]
  (let [current-id (state :current-player)
        players (state :players)
        p (players current-id)
        hand (p :hand)]
    (if (seq-contains? hand :clown)
      ;; TODO: Choose the person we know the least about (sort by highest sum of all probabilities)
      (list :clown (first (all-but-player state current-id)) nil)
      )))

(defn play-knight? [state]
  (let [current-id (state :current-player)
        players (state :players)
        p (players current-id)
        hand (p :hand)]
    (if (seq-contains? hand :knight)
      ;; TODO: Choose the person we are surest about having a higher card than (sort by sum of probabilities < other card in our hand)
      (list :knight (first (all-but-player state current-id)) nil)
      )))

;; TODO: priestess if there are lots of soldiers (can just check in deck)
;; knights are out there (can just check in deck) and your other card is low
;; someone clowned you & you just drew a priestess
(defn play-priestess? [state]
  (let [current-id (state :current-player)
        players (state :players)
        p (players current-id)
        hand (p :hand)]
    (if (and (seq-contains? hand :priestess))
      (list :priestess nil nil)
      )))

;; TODO: if you know someone has the princess if you can
;; wizard someone who likely has a higher card than your other card
;; wizard yourself if you have a low card late in the game & higher cards exist in deck?
(defn play-wizard? [state]
  (let [current-id (state :current-player)
        players (state :players)
        p (players current-id)
        hand (p :hand)]
    (if (seq-contains? hand :wizard)
      (list :wizard (first (all-but-player state current-id)) nil)
      )))

;; TODO: general: keep if 7+8 are out.
;; exchange if >x soldiers exist & you have princess?
(defn play-general? [state]
  (let [current-id (state :current-player)
        players (state :players)
        p (players current-id)
        hand (p :hand)]
    (if (seq-contains? hand :general)
      (list :general (first (all-but-player state current-id)) nil)
      )))

;; TODO: play minister if there are lots of 5+ cards left in the deck
(defn play-minister? [state]
  (let [current-id (state :current-player)
        players (state :players)
        p (players current-id)
        hand (p :hand)]
    (if (and (seq-contains? hand :minister))
      (list :minister nil nil)
      )))

;; Using the information we have, which card should I play?
;; returns (card-to-play target guess)
(defn pick-card-to-play [state]
  (reduce (fn [_ f] (let [x (f state)] (if (nil? x) nil (reduced x)))) nil
          [;; Attack priority
           play-soldier?
           play-knight?
           play-wizard?

           ;; Utility priority
           play-priestess?
           play-clown?
           play-general?
           play-minister?

           ;; Fallback: random card
           random-card]))

(defn knowledge? [state player-id target-id]
  "Does the player have knowledge on the target?"
  (let [players (state :players)
        p (players player-id)
        target-knowledge (nth (p :player-knowledge) target-id)]
    (not= nil (some #(not= 0 %) target-knowledge))))

(defn reset-player-knowledge [state player-id target-id]
  "A player could have anything in the deck."
  (let [players (state :players)
        p (players player-id)
        deck-knowledge (p :deck-knowledge)]
    (assoc-in state [:players player-id :player-knowledge target-id] deck-knowledge)))

(defn check-player-knowledge [state player-id target-id]
  "Reset the player's knowledge if necessary."
  ;; (omni "Does player %d have knowledge of %d? %s -- %s" player-id target-id (knowledge? state player-id target-id) (nth (((state :players) player-id) :player-knowledge) target-id))
  (if-not (knowledge? state player-id target-id)
    (reset-player-knowledge state player-id target-id)
    state))

(defn add-player-knowledge [state player-id target-id cards]
  "Add information about the target player to the player's knowledge. We can also remove a card from the deck knowledge if it's a single card."
  (let [players (state :players)
        p (players player-id)
        deck-knowledge (p :deck-knowledge)
        all-player-knowledge (p :player-knowledge)
        target-knowledge (all-player-knowledge target-id)]
    (assoc-in state [:players player-id :player-knowledge target-id]
              (into {} (for [[k v] target-knowledge]
                         [k (if (seq-contains? cards k) v 0)])))))

(defn remove-player-knowledge [state player-id target-id cards]
  "Remove information about the player."
  (let [players (state :players)
        current-id (state :current-player)
        current (players current-id)
        was-soldier? (= (current :last-played) :soldier)
        p (players player-id)
        target-knowledge ((p :player-knowledge) target-id)
        deck-knowledge (p :deck-knowledge)]
    ;; (omni "Removing %s from player %d's knowledge of player %d." cards player-id target-id)
    ;; (omni "Target knowledge: %s" (target-knowledge (first cards)))
    (cond-> state
      ;; The target player played a card. If the player thought that they could have had that card, reset knowledge. If it was 0, do nothing.
      (and (= 1 (count cards)) (= target-id current-id) (< 0 (target-knowledge (first cards)))) (assoc-in [:players player-id :player-knowledge target-id] deck-knowledge)
      ;; Soldier was played.
      (and (= 1 (count cards)) (not= target-id current-id) was-soldier?) (assoc-in [:players player-id :player-knowledge target-id (first cards)] 0)
      ;; The current player drew a card.
      (and (= 1 (count cards)) (not= target-id current-id) (not was-soldier?)) (assoc-in [:players player-id :player-knowledge target-id (first cards)] (max 0 (dec (target-knowledge (first cards)))))
      ;; A range of cards, so a knight was played.
      (< 1 (count cards)) (assoc-in [:players player-id :player-knowledge target-id]
                                    (into {} (for [[k v] target-knowledge]
                                               [k (if (seq-contains? cards k) 0 v)])))
      ;; true omni-state
      true (check-player-knowledge player-id target-id))))

(defn swap-player-knowledge [state a-id b-id]
  "Swap the knowledge everyone has about a and b."
  (let [players (state :players)]
    (reduce (fn [s v]
              (let [p (players v)
                    knowledge (p :player-knowledge)
                    a-knowledge (knowledge a-id)
                    b-knowledge (knowledge b-id)]
                (-> s
                    (assoc-in [:players v :player-knowledge a-id] b-knowledge)
                    (assoc-in [:players v :player-knowledge b-id] a-knowledge))))
            state
            (range (count players)))))

(defn start-game [num-players rule-set]
  {:pre [(< 1 num-players 5)
         (or (= rule-set :original) (= rule-set :tempest))]}
  "Start a new game of 2-4 players! Shuffle, burn a card, then deal to the number of players."
  (let [deck (shuffle full-deck)
        state {:status :begin
               :phase 0
               :current-player 0
               :rule-set rule-set
               :players (vec (take num-players (repeatedly player)))
               :deck deck}
        players (state :players)]
    (announce "A new game of %d players using the %s ruleset begins." num-players rule-set)
    (-> state
        initialize-knowledge
        burn-card
        ((partial reduce draw-card) (range num-players))
        (assoc :status :playing))))
