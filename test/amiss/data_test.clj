(ns amiss.data-test
  (:require [clojure.test :refer :all]
            [amiss.data :refer :all]))

(deftest test-make-players
  (testing "the right number of players are created"
    (is (= (count (make-players)) 4))
    (is (= (count (make-players 3)) 3))
    (is (= (count (make-players 2)) 2))
    (is (thrown? Exception (make-players 1)))
    (is (thrown? Exception (make-players 5))))
  (testing "the positions are correctly assigned"
    (is (= [0 1 2 3] (map :position (make-players))))
    )
  )
