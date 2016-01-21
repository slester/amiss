(ns amiss.util)

(defn remove-first [coll item] (let [sp (split-with (partial not= item) coll)] (concat (first sp) (rest (second sp)))))
(defn add-card [coll card] (conj coll card))
(defn remove-card [coll card] (remove-first coll card))
(defn seq-contains? [coll card] (boolean (some #(= % card) coll)))
(defn delete-element [v pos] (vec (concat (subvec v 0 pos) (subvec v (inc pos)))))
