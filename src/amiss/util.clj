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

(defn available-targets [state]
  "Find available targets for a targeted action."
  (map :position (filter #(and (not= (:position %) (:current-player state)) (:active %) (not= (:last-played %) :priestess)) (:players state))))

(defn active-players [state]
  "Find which players are still player"
  (map :position (filter :active (:players state))))

(defn game-over? [state]
  "Check if the game has ended."
  (or (= 1 (count (filter :active (:players state))))
      (= 0 (count (:deck state)))))
