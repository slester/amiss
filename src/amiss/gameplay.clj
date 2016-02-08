(ns amiss.gameplay)

(defmulti execute
  (fn [state command] (:type command)))

(defmethod execute :draw-card
  [state command]
  (let [player (:player command)
        deck (:deck state)
        card (first deck)
        new-deck (rest deck)]
    (println "Player" player "draws" card)
    (-> state
        (update-in [:players player :hand] conj card)
        ;; TODO: knowledge updates
        (assoc :deck new-deck))))

(defn do-commands [state commands]
  (reduce (fn [current-state command] (execute current-state command)) state commands))

(defn advance [state commands]
  (-> state
      (do-commands commands)
      ))
