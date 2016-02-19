(ns amiss.gameplay
  (:require [amiss.util :as u]))

;;---------------------------
;; game actions
;;---------------------------

(defmulti execute
  (fn [state command] (:type command)))

(defmethod execute :draw [state command]
  (let [player (:player command)
        deck (:deck state)
        card (first deck)
        new-deck (rest deck)]
    (println "Player" player "draws" card)
    (-> state
        ;; TODO: knowledge updates
        (update-in [:players player :hand] conj card)
        (assoc :deck new-deck))))

;; discard a card ;;
(defmethod execute :discard [state command]
  (let [player (:player command)
        card (:card command)]
    (-> state
        ;; TODO: knowledge updates
        ;; TODO: if card is princess?
        (update-in [:players player :hand] u/remove-first card)
        (update-in [:players player :discard] conj card))))

;; discard an entire hand ;;
(defmethod execute :discard-hand [state command]
  (let [player (:player command)
        hand (get-in state [:players player :hand])]
    (-> state
        ;; TODO: knowledge updates
        ;; TODO: if hand has princess?
        (assoc-in [:players player :hand] '())
        (update-in [:players player :discard] into hand))))

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

(defn do-commands [state commands]
  (reduce (fn [current-state command] (execute current-state command)) state commands))

;;---------------------------
;; game progression
;;---------------------------

;; start game ;;
(defn start-game [state]
  ;; all players draw a card
  (reduce #(execute % {:type :draw :player %2}) state (range (count (:players state)))))

;; has the game ended? ;;
(defn check-game-ending [state]
  (if (or
        (= 1 (count (filter :active (:players state))))
        (= 0 (count (:deck state))))
    (assoc-in state [:status] :game-over)
    state))

;; advance the turn to the next available player ;;
(defn advance-turn [state]
  (let [current (:current-player state)
        players (:players state)
        active (:position (filter :active players))
        next-player (second (drop-while (complement #{current}) (cycle active)))]
  (if (not= :game-over (:status state))
    (assoc state :current-player next-player)
    state)))

;; step through a turn ;;
(defn advance [state commands]
  (-> state
      (do-commands commands)
      (check-game-ending)
      (advance-turn)))
