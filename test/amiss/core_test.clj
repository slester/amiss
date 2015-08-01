(ns amiss.core-test
  (:require [clojure.test :refer :all]
            [amiss.core :refer :all]))

(deftest test-burning-deck
  (testing "The deck burns a card correctly."
    (let [state (start-game 4)
          deck (state :deck)
          burn-state (burn-card state)
          burn-state-deck (burn-state :deck) ]
      (is (not= deck burn-state-deck)))))

(deftest test-starting-game
  (testing "Starting a new game."
    (let [state (start-game 4)]
      (is (= 4 (count (state :players))))
      (is (= 11 (count (state :deck))))
      (is (= false (game-over? state)))
      )))

(deftest test-random-play
  (testing "Draw and discard a random card in the hand."
    (let [state (start-game 3)
          states (take-while (complement game-over?) (iterate play state))]
      (str (last states)))))

; This will sometimes fail because the minister is getting triggered.
(deftest test-drawing
  (testing "Drawing the entire deck."
    (let [state (start-game 4)]
      (loop [s state]
        (if (not= :over (s :status))
          (recur (-> s
                     (draw-card (s :current-player))
                     next-turn
                     end-game))
          (do
            ; The deck should be empty.
            (is (empty? (s :deck)))
            ; The burned card should still exist.
            (is (not= nil (s :burned-card)))
            ; Force player 1 to draw the burned card.
            (is (= nil ((draw-card s 1) :burned-card)))))))))

(deftest test-compare-cards
  (testing "Comparing equal cards."
    (is (= nil (compare-cards :wizard :wizard))))
  (testing "Comparing unequal cards, a > b."
    (is (= true (compare-cards :general :wizard))))
  (testing "Comparing unequal cards, a < b."
    (is (= false (compare-cards :priestess :wizard)))))

