(ns amiss.core
  (:require [clojure.core.async :refer [go timeout alts! <! >! chan]]
            [amiss.config :as config]
            [amiss.gameplay :as gameplay]
            [amiss.data :as data]))

(defonce is-dev? true)

(def total-players 4)
(def game-over? (atom false))

;; players put commands onto the command chan
;; and read from the state-chan to update their internal state
(defn game [command-chan state-chan]
  (let [initial-state (gameplay/start-game (data/make-state total-players))]
  (go
    (loop [state initial-state
           commands []
           timer (timeout config/fpms)]
      (let [[v c] (alts! [timer command-chan] :priority true)]
       (condp = c
         command-chan (when v (recur state (conj commands v) timer))
         timer (let [new-state (gameplay/advance state commands)]
                 ;; broadcast the new state
                 ;; (>! state-chan new-state)
                 (recur new-state [] (timeout config/fpms)))))))))

(defn new-game! []
  {:command-chan (chan)
   :state-chan (chan)})

(defn -main []
  (println "in main")
  (let [{:keys [command-chan state-chan]} (new-game!)]
    (game command-chan state-chan)
    ;; (go
    ;;     )
    (while true)
    ))
