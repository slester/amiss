(ns amiss.knowledge
  (:require [amiss.util :as u]
            [amiss.config :as cfg]
            [amiss.data :as d]))

(defn remove-card [knowledge card]
  (println knowledge card)
  (if (contains? knowledge card)
    (update knowledge card (comp #(max 0 %) dec))
    knowledge))

(defn set-as [state player target hand]
  "Sets a player's knowledge of another player."
  (assoc-in state [:players player :player-knowledge target] (hash-map (first hand) 1)))

(defn remove-from-deck [state player cards]
  {:pre [(coll? cards)]}
  "Remove cards from the player's knowledge about the deck."
  (let [deck-knowledge (get-in state [:players player :deck-knowledge])
        new-knowledge (reduce #(remove-card % %2) deck-knowledge cards)]
    (assoc-in state [:players player :deck-knowledge] new-knowledge)))

(defn remove-from-player [state player target cards]
  {:pre [(coll? cards)]}
  "Remove cards from the player's knowledge about the given target."
  (println "removing" cards "from player" player)
  (let [player-knowledge (get-in state [:players player :player-knowledge target])
        new-knowledge (reduce #(remove-card % %2) player-knowledge cards)]
    (assoc-in state [:players player :player-knowledge target] new-knowledge)))

;; TODO remove-from-all and remove-from-others are confusingly named
(defn drew-card [state player card]
  "Remove card from a player's knowledge of all of the players as well as their deck knowledge."
  (as-> state s
      (remove-from-deck s player [card])
      (reduce #(remove-from-player % player %2 [card]) s (u/other-player-positions s player))))

(defn discarded-card [state player cards]
  {:pre [(coll? cards)]}
  "Remove cards from all of the other players' knowledge about the given player as well as from their deck knowledge."
  (println "removing" cards "from players except" player)
  (as-> state s
      (reduce #(remove-from-deck % %2 cards) s (u/other-player-positions s player))
      (reduce #(remove-from-player % %2 player cards) s (u/other-player-positions s player))))

(defn reset [state player target]
  "Reset the player's knowledge about the given target to what the player knows about the deck."
  (let [deck-knowledge (get-in state [:players player :deck-knowledge])]
    (assoc-in state [:players player :player-knowledge target] deck-knowledge)))

(defn swap [state player-a player-b]
  "Swaps everyone's knowledge about two players. The two players then know exactly what the other has."
  (let [a-hand (get-in state [:players player-a :hand])
        b-hand (get-in state [:players player-b :hand])]
    (as-> state x
        ;; player a knows that player b has a's hand
        (assoc-in x [:players player-a :player-knowledge player-b] (frequencies a-hand))
        ;; and vice-versa
        (assoc-in x [:players player-b :player-knowledge player-a] (frequencies b-hand))
        ;; all other players swap their knowledge of a and b
        (reduce (fn [s p]
                  (let [a-knowledge (get-in s [:players p :player-knowledge player-a])
                        b-knowledge (get-in s [:players p :player-knowledge player-b])]
                    (-> s
                        (assoc-in [:players p :player-knowledge player-a] b-knowledge)
                        (assoc-in [:players p :player-knowledge player-b] a-knowledge))))
                x
                (u/all-players-except state player-a player-b)))))
