(ns amiss.core-test
  (:require [clojure.test :refer :all]
            [amiss.core :refer :all]))

;; (deftest test-burning-deck
;;   (testing "The deck burns a card correctly."
;;     (let [state (start-game 4 :original)
;;           deck (state :deck)
;;           burn-state (burn-card state)
;;           burn-state-deck (burn-state :deck) ]
;;       (is (not= deck burn-state-deck)))))
;;
;; (deftest test-starting-game
;;   (testing "Starting a new game."
;;     (let [state (start-game 4 :original)]
;;       (is (= 4 (count (state :players))))
;;       (is (= 11 (count (state :deck))))
;;       (is (= false (game-over? state))))))

(deftest test-random-play
  (testing "Draw and discard a random card in the hand."
    (let [state (start-game 4 :original)
          states (split-with (complement game-over?) (iterate play state))]
      (final-summary (first (second states))))))

;; UTILITIES
(deftest test-compare-cards
  (testing "Comparing equal cards."
    (is (= nil (compare-cards :wizard :wizard))))
  (testing "Comparing unequal cards, a > b."
    (is (= true (compare-cards :general :wizard))))
  (testing "Comparing unequal cards, a < b."
    (is (= false (compare-cards :priestess :wizard)))))

