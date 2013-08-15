(ns chess.core-test
  (:require [clojure.test :refer :all]
            [chess.core :refer :all]))

(deftest test-parse-board
  (testing "parse-board should be able to parse a board"
    (doseq [board [(parse-board ["rnbqkbnr"
                                 "pppppppp"
                                 "........"
                                 "........"
                                 "........"
                                 "........"
                                 "PPPPPPPP"
                                 "RNBQKBNR"])
                   chess.core/board]]
      (is (= 8 (count board))
          "Board should have 8 ranks")
      (is (= (repeat 8 8)
             (map count board))
          "Each rank should have 8 files")
      (is (= (repeat 8 :black)
             (map :color (first board)))
          "First rank should have 8 black pieces") 
      (is (= (repeat 8 :white)
             (map :color (last board)))
          "Last rank should have 8 white pieces"))))
