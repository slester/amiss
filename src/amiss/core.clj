(ns amiss.core
  (:require [clojure.core.async :refer [go timeout alts! <! >! chan]]
            [amiss.config :as config]
            [amiss.gameplay :as gameplay]
            [amiss.data :as data]))

(def total-players 4)

;; players put commands onto the command chan
;; and read from the state-chan to update their internal state
(defn game [command-chan state-chan]
  (let [initial-state (gameplay/start-game (data/make-state total-players))]
    (go
      (>! state-chan initial-state)
      (loop [state initial-state
             commands []
             timer (timeout config/fpms)]
        (let [[v c] (alts! [timer command-chan] :priority true)]
          (condp = c
            command-chan (when v (recur state (conj commands v) timer))
            timer (let [new-state (gameplay/do-commands state commands)]
                    ;; broadcast the new state
                    (>! state-chan new-state)
                    (recur new-state [] (timeout config/fpms)))))))))

(defn new-game! []
  {:command-chan (chan)
   :state-chan (chan)})

(defn test-game [command-chan state-chan]
  (go
   (loop [s (<! state-chan)]
     (when (= :in-progress (:status s))
      (>! command-chan {:type :begin-turn})
      (>! command-chan {:type :play-random})
      (>! command-chan {:type :end-turn})
      (recur (<! state-chan))))
   (println "game over")
   ))

(defn -main []
  (println "in main")
  (let [{:keys [command-chan state-chan]} (new-game!)]
    ;; listen for events
    (game command-chan state-chan)
    (test-game command-chan state-chan)
    (while true)))
