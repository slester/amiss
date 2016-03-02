(ns amiss.knowledge
  (:require [amiss.util :as u]
            [amiss.config :as cfg]
            [amiss.data :as d]))

(defn remove-card [knowledge card]
  (update knowledge card (comp #(max 0 %) dec)))

(defn remove-from-deck [state player items]
  "Remove items from the player's knowledge about the deck."
  (let [cards (if (seq? items) items (list items))
        deck-knowledge (get-in state [:players player :deck-knowledge])
        new-knowledge (reduce #(remove-card % %2) deck-knowledge cards)]
    (assoc-in state [:players player :deck-knowledge] new-knowledge)))

(defn remove-from-player [state player target items]
  "Remove items from the player's knowledge about the given target."
  (let [cards (if (seq? items) items (list items))
        player-knowledge (get-in state [:players player :player-knowledge target])
        new-knowledge (reduce #(remove-card % %2) player-knowledge cards)]
    (assoc-in state [:players player :player-knowledge target] new-knowledge)))

(defn remove-from-other-players [state player items]
  "Remove items from all of the other players' knowledge about the given player as well as from their deck knowledge."
  (let [cards (if (seq? items) items (list items))]
    (-> state
        (remove-from-deck player items)
        (->> (reduce #(remove-from-player % player %2 cards) (u/other-player-positions state player))))))

(defn reset [state player target]
  "Reset the player's knowledge about the given target to what the player knows about the deck."
  (let [deck-knowledge (get-in state [:players player :deck-knowledge])]
    (assoc-in state [:players player :player-knowledge target] deck-knowledge)))

