(ns amiss.core-test
  (:require [clojure.test :refer :all]
            [amiss.core :refer :all]))

(deftest test-shuffling-deck
  (testing "The deck shuffles correctly."
    (is (not= deck (shuffle deck)))))

(deftest test-compare-cards
  (testing "Comparing equal cards."
    (is (= nil (compare-cards :wizard :wizard))))
  (testing "Comparing unequal cards, a > b."
    (is (= true (compare-cards :general :wizard))))
  (testing "Comparing unequal cards, a < b."
    (is (= false (compare-cards :priestess :wizard)))))

(deftest test-court-powers
  (testing "Is the princess a princess?"
    (is (= true (is-princess? :princess))))
  (testing "Is a non-princess card a princess?"
    (is (= false (is-princess? :knight)))))
