(ns amiss.data)

(defonce court-values
  {:princess  8
   :minister  7
   :general   6
   :wizard    5
   :priestess 4
   :knight    3
   :clown     2
   :soldier   1})

(defonce court-deck
  (flatten
    [:princess
     :minister
     :general
     (repeat 2 :wizard)
     (repeat 2 :priestess)
     (repeat 2 :knight)
     (repeat 2 :clown)
     (repeat 5 :soldier)]))

(defrecord Player
  [active
   position
   hand
   discard
   last-played
   deck-knowledge
   player-knowledge])

(defn make-deck-knowledge []
  "Instantiate a player's knowledge of other players"
  (frequencies court-deck))

(defn make-player-knowledge [total-players position]
  "Instantiate a player's knowledge of other players."
  (vec (repeat total-players (frequencies court-deck))))

(defn make-players
  ;; default to 4 players
  ([] (make-players 4))
  ([n]
   (when-not (< 1 n 5) (throw (Exception. "Only 2-4 players are allowed.")))
   (map #(->Player true % '() '() nil (make-deck-knowledge) (make-player-knowledge n %)) (range n))))
