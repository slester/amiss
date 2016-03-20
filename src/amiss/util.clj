(ns amiss.util)

(defn remove-first [coll item]
 "Remove the first instance of a given item from a sequence."
  (let [sp (split-with (partial not= item) coll)] (concat (first sp) (rest (second sp)))))

(defn seq-contains? [coll item]
  "Does the sequence contain an item?"
  (boolean (some #(= % item) coll)))

(defn delete-element [v pos]
  "Delete an element at a given position."
  (vec (concat (subvec v 0 pos) (subvec v (inc pos)))))

(defn max-keys [coll]
  (keys (filter #(= (val %) (apply max (vals coll))) coll)))

(defn card-values [values cards]
  (apply + (map #(% values) cards)))

(defn other-player-positions [state player]
  (map :position (filter #(and (:active %) (not= player (:position %))) (:players state))))

(defn available-target-positions [state]
  "Find available targets for a targeted action."
  (map :position (filter #(and (not= (:position %) (:current-player state)) (:active %) (not= (:last-played %) :priestess)) (:players state))))

(defn active-players [state]
  "Find which players are still player"
  (filter :active (:players state)))

(defn active-player-positions [state]
  "Same as active-players, but returning positions instead of Player records."
  (map :position (active-players state)))

(defn all-players-except [state & excluded-players]
  (let [players (active-player-positions state)]
    (into '() (clojure.set/difference (set players) (set excluded-players)))))

(defn is-active [state player]
  (get-in state [:players player :active]))

(defn game-over? [state]
  "Check if the game has ended."
  (or (= 1 (count (filter :active (:players state))))
      (= 0 (count (:deck state)))))
