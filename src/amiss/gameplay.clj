(ns amiss.gameplay
  (:require [amiss.util :as u]
            [amiss.data :as data]))

(declare check-princess)

;;---------------------------
;; game actions
;;---------------------------

(defmulti execute
  (fn [state command] (:type command)))

(defmethod execute :draw [state command]
  (let [player (:player command)
        active? (get-in state [:players player :active])
        deck (:deck state)
        card (first deck)
        new-deck (rest deck)]
    (println "Player" player "draws" card)
    (if active?
      (-> state
          ;; TODO: knowledge updates
          (update-in [:players player :hand] conj card)
          (assoc :deck new-deck))
      state)))

;; play a card ;;
(defmethod execute :play [state command]
  (let [player (:current-player state)
        card (:card command)]
    (-> state
        ;; TODO: knowledge updates
        (update-in [:players player :hand] u/remove-first card)
        (assoc-in [:players player :last-played] card)
        (update-in [:players player :discard] conj card)
        (check-princess)
        )))

;; discard a card ;;
(defmethod execute :discard [state command]
  (let [player (:player command)
        card (:card command)]
    (-> state
        ;; TODO: knowledge updates
        (update-in [:players player :hand] u/remove-first card)
        (update-in [:players player :discard] conj card)
        (check-princess)
        )))

;; discard an entire hand ;;
(defmethod execute :discard-hand [state command]
  (let [player (:player command)
        hand (get-in state [:players player :hand])]
    (-> state
        ;; TODO: knowledge updates
        (assoc-in [:players player :hand] '())
        (update-in [:players player :discard] into hand)
        (check-princess)
        )))

;; swap hands ;;
(defmethod execute :swap-hands [state command]
  (let [a (:player-a command)
        b (:player-b command)
        a-hand (get-in state [:players a :hand])
        b-hand (get-in state [:players b :hand])]
    (-> state
        ;; TODO: knowledge updates
        (assoc-in [:players a :hand] b-hand)
        (assoc-in [:players b :hand] a-hand))))

;; show hand of player b to player a ;;
(defmethod execute :show-hand [state command]
  (let [a (:player-a command)
        b (:player-b command)
        b-hand (get-in state [:players b :hand])]
    (-> state
        ;; TODO: knowledge updates
        )))

;; compare hands ;;
(defmethod execute :compare-hands [state command]
  (let [a (:player-a command)
        b (:player-b command)
        a-val ((first (get-in state [:players a :hand])) data/court-values)
        b-val ((first (get-in state [:players b :hand])) data/court-values)]
    (cond
      (< a-val b-val) (execute state {:type :remove-player :player a})
      (> a-val b-val) (execute state {:type :remove-player :player b})
      :else state ;; TODO does this do anything else?
      )
    ))

(defmethod execute :guess [state command]
  ;; TODO
  state
  )

;; remove a player from the game ;;
(defmethod execute :remove-player [state command]
  (let [player (:player command)]
  (-> state
      (assoc-in [:players player :active] false))))

(defmethod execute :end-turn [state command]
  "End the current player's turn and advance to the next available player."
  (let [current (:current-player state)
        players (:players state)
        active (:position (filter :active players))
        next-player (second (drop-while (complement #{current}) (cycle active)))
        game-over (u/game-over? state)]
  (if game-over
    (assoc state :status :game-over)
    (assoc state :current-player next-player))))

(defn do-commands [state commands]
  (reduce (fn [current-state command] (execute current-state command)) state commands))

;;---------------------------
;; card actions
;;---------------------------

(defmulti card-action
  (fn [state command] (:card command)))

(defmethod card-action :soldier [state command]
  (-> state
      (execute {:type :play :card :soldier}))
      (execute {:type :guess :player-a (:player-a command) :player-b (:player-b command) :guessed-card (:guessed-card command)}))

(defmethod card-action :clown [state command]
  (-> state
      (execute {:type :play :card :clown})
      (execute {:type :show-hand :player-a (:player-a command) :player-b (:player-b command)})))

(defmethod card-action :knight [state command]
  (-> state
      (execute {:type :play :card :knight})
      (execute {:type :compare-hand :player-a (:player-a command) :player-b (:player-b command)})))

(defmethod card-action :priestess [state command]
  (execute state {:type :play :card :priestess}))

(defmethod card-action :wizard [state command]
  (-> state
      (execute {:type :play :card :wizard})
      (execute {:type :discard-hand :player (:player command)})
      (execute {:type :draw :player (:player command)})))

(defmethod card-action :general [state command]
  (-> state
      (execute {:type :play :card :minister})
      (execute {:type :swap-hands :player-a (:player-a command) :player-b (:player-b command)})))

(defmethod card-action :minister [state command]
  (execute state {:type :play :card :general}))

(defmethod card-action :princess [state command]
  (execute state {:type :play :card :princess}))

;;---------------------------
;; game status checks
;;---------------------------

(defn check-princess [state]
  "Check if a player should be out for discarding the princess."
  (if-let [player (:position (filter #(u/seq-contains? (:discard %) :princess) (:players state)))]
    (execute state {:type :remove-player :player player})
    state))

;;---------------------------
;; game progression
;;---------------------------

(defn start-game [state]
  "Perform start of game actions."
  ;; all players draw a card
  (reduce #(execute % {:type :draw :player %2}) state (range (count (:players state)))))
