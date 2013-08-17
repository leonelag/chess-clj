(ns chess.core-test
  (:require [clojure.test :refer :all]
            [clojure.string]
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

(deftest test-rank
  (testing "Rank and file functions"
    (is (= 0
           (file \a))
        "Rank A is white player's leftmost")
    (is (= 7
           (file \h))
        "Rank H is white player's rightmost")
    (is (= 0
           (rank \8))
        "Rank 8 is furthest from white player and first element in board representation")
    (is (= 7
           (rank \1))
        "Rank 1 is closests to white player and 7th element in board representation")
    (is (= {:kind  :king
            :color :black}
           (select-keys (piece-at board (file \e) (rank \8))
                        [:kind :color]))
        "Piece at position E8 of the initial board should be the white king.")
    (is (= {:kind  :king
            :color :white}
           (select-keys (piece-at board (file \e) (rank \1))
                        [:kind :color]))
        "Piece at position E1 of the initial board should be the white king.")))

(deftest test-print-board
  (testing ""
    (is (= (with-out-str (print-board chess.core/board))
           (clojure.string/join
            (interleave ["rnbqkbnr" 
                         "pppppppp"
                         "........"
                         "........"
                         "........"
                         "........"
                         "PPPPPPPP"
                         "RNBQKBNR"]
                        (repeat \newline)))))))

(deftest test-parse-move-pawns
  (testing "Testing parse-move for valid moves of pawns."
    (let [mv (parse-move "e5")]
      (is (= :pawn
             (:piece mv)))
      (is (= [(file \e) (rank \5)]
             (:to mv)))
      (is (not (:ep mv)))
      (is (not (:cap mv))))
    (let [mv (parse-move "exd4")]
      (is (= :pawn
             (:piece mv)))
      (is (= [(file \d) (rank \4)]
             (:to mv)))
      (is (= (file \e)
             (:from-file mv)))
      (is (not (:ep mv)))
      (is (:cap mv)))
    (let [mv (parse-move "exd4(ep)")]
      (is (= :pawn
             (:piece mv)))
      (is (= [(file \d) (rank \4)]
             (:to mv)))
      (is (= (file \e)
             (:from-file mv)))
      (is (:ep mv))
      (is (:cap mv)))))
