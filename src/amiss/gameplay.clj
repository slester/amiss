(ns amiss.gameplay
  (:require [amiss.util :as u]
            [amiss.data :as data]))

(declare card-action)
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
  (let [current (:current-player state)
        card (:card command)]
    (println "Player" current "plays" card)
    (-> state
        ;; TODO: knowledge updates
        (update-in [:players current :hand] u/remove-first card)
        (assoc-in [:players current :last-played] card)
        (update-in [:players current :discard] conj card)
        (check-princess)
        )))

;; play a random card ;;
(defmethod execute :play-random [state command]
  (let [current (:current-player state)
        players (map :position (:players state))
        card (rand-nth (get-in state [:players current :hand]))]
    (println "Player" current "plays" card)
    (card-action state {:card card
                        :target (rand-nth (u/available-targets state))
                        :player (rand-nth (u/active-players state))
                        :guessed-card (rand-nth (filter #(not= :soldier %) (keys data/court-values)))
                        })
    ))

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
  (let [current (:current-player state)
        target (:target command)
        current-hand (get-in state [:players current :hand])
        target-hand (get-in state [:players target :hand])]
    (-> state
        ;; TODO: knowledge updates
        (assoc-in [:players current :hand] target-hand)
        (assoc-in [:players target :hand] current-hand))))

;; show hand of player b to player a ;;
(defmethod execute :show-hand [state command]
  (let [current (:current-player state)
        target (:target command)
        target-hand (get-in state [:players target :hand])]
    (-> state
        ;; TODO: knowledge updates
        )))

;; compare hands ;;
(defmethod execute :compare-hands [state command]
  (let [current (:current-player state)
        target (:target command)
        current-val ((first (get-in state [:players current :hand])) data/court-values)
        target-val ((first (get-in state [:players target :hand])) data/court-values)]
    (cond
      (< current-val target-val) (execute state {:type :remove-player :player current})
      (> current-val target-val) (execute state {:type :remove-player :player target})
      :else state ;; TODO does this do anything else?
      )))

(defmethod execute :guess [state command]
  ;; TODO
  state
  )

;; remove a player from the game ;;
(defmethod execute :remove-player [state command]
  (let [player (:player command)]
    (println "Removing" player)
    (-> state
        (assoc-in [:players player :active] false))))

(defmethod execute :begin-turn [state command]
  "Begin a player's turn."
  (let [player (:current-player state)]
    (println "It is now player" (str player "'s turn."))
    (-> state
        (execute {:type :draw :player player}))
    ))

(defmethod execute :end-turn [state command]
  "End the current player's turn and advance to the next available player."
  (let [current (:current-player state)
        players (:players state)
        active (u/active-players state)
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
  (fn [state command]

  (println command)
    (:card command)))

(defmethod card-action :soldier [state command]
  (-> state
      (execute {:type :play :card :soldier})
      (execute {:type :guess :target (:target command) :guessed-card (:guessed-card command)})))

(defmethod card-action :clown [state command]
  (-> state
      (execute {:type :play :card :clown})
      (execute {:type :show-hand :target (:target command)})))

(defmethod card-action :knight [state command]
  (-> state
      (execute {:type :play :card :knight})
      (execute {:type :compare-hands :target (:target command)})))

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
      (execute {:type :swap-hands :target (:target command)})))

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
