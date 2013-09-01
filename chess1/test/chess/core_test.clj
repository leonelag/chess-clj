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
             (map :color (last board)))
          "Last seq in the data structure should have 8 black pieces")
      (is (= (repeat 8 :white)
             (map :color (first board)))
          "First seq in the data structure should have 8 white pieces")
      (is (= [:rook :knight :bishop :queen :king :bishop :knight :rook]
             (map :kind (first board ))
             (map :kind (last board)))
          "First and last ranks of the board should have the correct pieces.")
      (is (= (repeat 8 :pawn)
             (map :kind (second board))
             (map :kind (nth board 6)))
          "Second and seventh ranks of the board should only have pawns."))))

(deftest test-rank
  (testing "Rank and file functions"
    (is (= 0
           (file \a))
        "Rank A is white player's leftmost")
    (is (= 7
           (file \h))
        "Rank H is white player's rightmost")
    (is (= 7
           (rank \8))
        "Rank 8 is furthest from white player and last element in board data structure")
    (is (= 0
           (rank \1))
        "Rank 1 is closest to white player and first element in board data structure")
    (is (= {:kind  :king
            :color :black}
           (select-keys (piece-at board "E8")
                        [:kind :color]))
        "Piece at position E8 of the initial board should be the white king.")
    (is (= {:kind  :king
            :color :white}
           (select-keys (piece-at board "E1")
                        [:kind :color]))
        "Piece at position E1 of the initial board should be the white king.")))

(deftest test-to-std-notation
  (is (= "E1" (to-std-notation 0 4)))
  (is (= "A1" (to-std-notation 0 0)))
  (is (= "H1" (to-std-notation 0 7)))
  (is (= "A8" (to-std-notation 7 0)))
  (is (= "H8" (to-std-notation 7 7)))
  (is (= "E8" (to-std-notation 7 4))))


(deftest test-print-board
  (testing ""
    (is (= (with-out-str (print-board chess.core/board))
           (clojure.string/join
            (interleave ["8 |rnbqkbnr"
                         "7 |pppppppp"
                         "6 |........"
                         "5 |........"
                         "4 |........"
                         "3 |........"
                         "2 |PPPPPPPP"
                         "1 |RNBQKBNR"
                         "   --------"
                         "   abcdefgh"]
                        (repeat \newline)))))))

;;
;; TODO - rewrite test to use new function parse-eval-move instead of parse-move
;;

;; (deftest test-parse-move-pawns
;;   (testing "Testing parse-move for valid moves of pawns."
;;     (let [mv (parse-move "e5")]
;;       (is (= :pawn
;;              (:piece mv)))
;;       (is (= (parse-square "e5")
;;              (:to mv)))
;;       (is (not (:ep mv)))
;;       (is (not (:cap mv))))
;;     (let [mv (parse-move "exd4")]
;;       (is (= :pawn
;;              (:piece mv)))
;;       (is (= (parse-square "d4")
;;              (:to mv)))
;;       (is (= (file \e)
;;              (:from-file mv)))
;;       (is (not (:ep mv)))
;;       (is (:cap mv)))
;;     (let [mv (parse-move "exd4(ep)")]
;;       (is (= :pawn
;;              (:piece mv)))
;;       (is (= (parse-square "d4")
;;              (:to mv)))
;;       (is (= (file \e)
;;              (:from-file mv)))
;;       (is (:ep mv))
;;       (is (:cap mv)))))

(deftest test-player-pieces
  (testing "Testing player pieces"
    (is (empty? (filter nil?
                        (player-pieces chess.core/board
                                       :white)))
        "Should not return seq with nil elements.")
    (is (= (repeat 16 :white)
           (map :color (player-pieces chess.core/board
                                      :white)))
        "Should return 16 white pieces")
    (is (= #{0 1}
           (set (map :row (player-pieces chess.core/board
                                         :white))))
        "White pieces should be in rows 0 and 1.")
    (is (= #{6 7}
           (set (map :row (player-pieces chess.core/board
                                         :black))))
        "Black pieces should be in rows 6 and 7.")))

(deftest test-parse-square
  (testing "Testing parse-square"
    (is (= [5 2] (parse-square "c6")))
    (is (= [2 2] (parse-square "c3")))
    (is (= [2 5] (parse-square "f3")))
    (is (= [3 4] (parse-square "e4")))
    (is (= [4 4] (parse-square "e5")))))

(deftest test-knight-squares
  (testing "Testing knight-squares"
    (is (= (map parse-square ["F5" "E6" "C6" "B5" "B3" "C2" "E2" "F3"])
           (apply knight-squares (parse-square "D4")))
        "A knight at D4 can move to F5,E6,C6,B5,B3,C2,E2,F3")
    (is (= (map parse-square ["F7" "G6"])
           (apply knight-squares (parse-square "H8")))
        "A knight at H8 can move to F7,G6")
    (is (= (map parse-square ["C2" "B3"])
           (apply knight-squares (parse-square "A1")))
        "A knight at H8 can move to C2,B3")))

(deftest test-diagonals
  (testing "Testing diagonals"
    (is (= [[[0 0] [1 1] [2 2]]
            [[2 4] [1 5] [0 6]]
            [[4 2] [5 1] [6 0]]
            [[4 4] [5 5] [6 6] [7 7]]])
        "Diagonals starting from 3,3")
    (is (= [[[1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7]]]
           (diagonals 0 0))
        "Diagonals starting from lower-left corner of the board.")
    (is (= [[[6 6] [5 5] [4 4] [3 3] [2 2] [1 1] [0 0]]]
           (diagonals 7 7))
        "Diagonals starting from upper-right corner of the board.")))

(deftest test-parallels
  (testing "Testing parallels."
    (is (= [[[3 4] [3 5] [3 6] [3 7]]
            [[4 3] [5 3] [6 3] [7 3]]
            [[3 2] [3 1] [3 0]]
            [[2 3] [1 3] [0 3]]]
           (parallels 3 3))
        "Squares attacked by a rook on 3,3")
    (is (= [[[0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7]]
            [[1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0]]]
           (parallels 0 0))
        "Squares attacked by a rook on 0,0")
    (is (= [[[7 6] [7 5] [7 4] [7 3] [7 2] [7 1] [7 0]]
            [[6 7] [5 7] [4 7] [3 7] [2 7] [1 7] [0 7]]]
           (parallels 7 7))
        "Squares attacked by a rook on 7,7")))

(deftest test-square-attacked
  (testing "Testing square-attacked for different board configurations"
    (let [board (parse-board ["xxxxxxxR"
                              "xQxxP.xx"
                              "xxx....."
                              ".x.x...p"
                              "xxx.x.x."
                              ".x.x.x.."
                              ".N.x.xx."
                              "...xP..x"])]
      (is (square-attacked? board :white [6 0])
          "white queen")
      (is (square-attacked? board :white [7 0])
          "white queen")
      (is (square-attacked? board :white [6 2])
          "white queen")
      (is (square-attacked? board :white [4 3])
          "white queen")
      (is (square-attacked? board :white [7 5])
          "white rook")
      (is (square-attacked? board :white [0 3])
          "white knight")
      (is (square-attacked? board :white [2 3])
          "white knight")
      (is (square-attacked? board :white [3 2])
          "white knight")
      (is (square-attacked? board :white [1 3])
          "white pawn")
      (is (square-attacked? board :white [1 5])
          "white pawn")

      (doseq [color [:white :black]]
        (is (not (square-attacked? board color [0 6]))
            "not attacked")
        (is (not (square-attacked? board color [1 4]))
            "pawn does not attack forward")
        (is (every? #(not (square-attacked? board color %))
                    [[4 0] [4 2]])
            "knight's move from the queen")
        (is (not (square-attacked? board color [4 5]))
            "not attacked")
        (is (not (square-attacked? board color [5 6]))
            "black pawn does not attack south")
        (is (not (square-attacked? board color [0 1]))
            "hidden from queen behind knight")
        (is (not (square-attacked? board color [6 5]))
            "hidden from queen behind pawn")))))

(deftest test-board-move
  "Test moves on the game."
  ; pawn to e3
  (let [b (board-move board {:move [[1 4] [2 4]]})]
    (is (nil? (piece-at b 1 4)))
    (is (= (select-keys (piece-at board 1 4)
                        [:kind :color])
           (select-keys (piece-at b 2 4)
                        [:kind :color]))))

  (let [b1 (parse-board ["...r...."
                         "....PK.."
                         ".k......"
                         "........"
                         "........"
                         "........"
                         "........"
                         "........"])
        b2 (board-move b1 {:remove [[6 4] [7 3]]
                           :add [[7 3 {:kind :queen
                                       :color :white
                                       :row 7
                                       :col 3
                                       :str \Q}]]})]
    (is (nil? (piece-at b2 6 4)))
    (is (= {:color :white,
            :kind  :queen}
           (select-keys (piece-at b2 7 3)
                        [:kind :color])))))

(deftest test-possible-moves
  (testing "Test possible moves"
    ;; moves for white
    (is (= (set (concat
                 ;; pawns
                 (apply concat
                        ;; on the starting board, all pawns can move to row 2 or 3

                        (for [col (range 8)]
                          [{:move [[1 col] [2 col]]}
                           {:move [[1 col] [3 col]]}]))

                 ;; knights
                 [{:move [[0 1] [2 0]]}
                  {:move [[0 2] [2 2]]}
                  {:move [[0 6] [2 5]]}
                  {:move [[0 6] [2 7]]}]))
           (set (possible-moves (new-game)
                                :white))))

    ;; moves for black after white has played e4
    (let [game (-> (new-game)
                   (move {:move [[1 4] [3 4]]}))]
      (is (= (set (concat
                   ;; black pawns
                   (apply concat
                          (for [col (range 8)]
                            [{:move [[6 col] [5 col]]}
                             {:move [[6 col] [4 col]]}]))
                   ;; black knights
                   [{:move [[7 1] [5 0]]}
                    {:move [[7 1] [5 2]]}
                    {:move [[7 6] [5 5]]}
                    {:move [[7 6] [5 7]]}]))
             (set (possible-moves game :black)))))))

(deftest test-check?
  (testing "Test function check?"
    (let [board (parse-board ["r.bqkbnr"
                              "ppp..Qpp"
                              "..np...."
                              "....p..."
                              "..B.P..."
                              "........"
                              "PPPP.PPP"
                              "RNB.K.NR"])]
      (is (check? board :black))
      (is (not (check? board :white))))))

(deftest test-checkmate?
  (testing "Test function checkmate?"
    ;; scholar's mate
    ;; http://en.wikipedia.org/wiki/Scholar%27s_mate
    (let [scholar (parse-board ["r.bqkb.r"
                                "pppp.Qpp"
                                "..n..n.."
                                "....p..."
                                "..B.P..."
                                "........"
                                "PPPP.PPP"
                                "RNB.K.NR"])]
      (is (check? scholar :black))
      (is (not (check? scholar :white)))
      (is (checkmate? scholar :black))
      (is (not (checkmate? scholar :white))))

    ;; Fool's mate
    ;; http://en.wikipedia.org/wiki/Fool%27s_mate
    (let [fools (parse-board ["rnb.kbnr"
                              "pppp.ppp"
                              "........"
                              "....p..."
                              "......Pq"
                              ".....P.."
                              "PPPPP..P"
                              "RNBQKBNR"])]
      (is (check? fools :white))
      (is (checkmate? fools :white))
      (is (check? fools :black))
      (is (checkmate? fools :black)))))

(deftest test-possible-moves-piece
  (testing "Test function possible-moves"
    (let [board (parse-board ["rnbqk.n."
                              "ppp...pP"
                              "...b...."
                              "...ppp.."
                              "..P.P..."
                              "P.N....."
                              ".P.P.PP."
                              "R.BQKBNR"])]
      ;; pawns
      (is (= [{:move [[2 0] [3 0]]}]
             (possible-moves-piece (piece-at board 2 0) board)))
      (is (= [{:move [[1 1] [2 1]]}
              {:move [[1 1] [3 1]]}]
             (possible-moves-piece (piece-at board 1 1) board)))
      (is (= [{:move [[3 2] [4 2]]}
              {:move [[3 2] [4 3]]}]
             (possible-moves-piece (piece-at board 3 2) board)))
      (is (= [{:move [[1 3] [2 3]]}
              {:move [[1 3] [3 3]]}]
             (possible-moves-piece (piece-at board 1 3) board)))
      (is (= [{:move [[3 4] [4 3]]}
              {:move [[3 4] [4 5]]}]
             (possible-moves-piece (piece-at board 3 4) board)))
      (is (= [{:move [[1 5] [2 5]]}
              {:move [[1 5] [3 5]]}]
             (possible-moves-piece (piece-at board 1 5) board)))
      (is (= [{:move [[1 6] [2 6]]}
              {:move [[1 6] [3 6]]}]
             (possible-moves-piece (piece-at board 1 6) board)))
      (is (= [{:move [[6 7] [7 7]]}
              {:move [[6 7] [7 6]]}]
             (possible-moves-piece (piece-at board 6 7) board)))
      ;; knight
      (is (= (set [{:move [[2 2] [1 0]]}
                   {:move [[2 2] [0 1]]}
                   {:move [[2 2] [1 4]]}
                   {:move [[2 2] [4 3]]}
                   {:move [[2 2] [4 1]]}
                   {:move [[2 2] [3 0]]}])
             (set (possible-moves-piece (piece-at board 2 2)
                                        board))))
      (is (= (set [{:move [[7 1] [5 0]]}
                   {:move [[7 1] [5 2]]}
                   {:move [[7 1] [6 3]]}])
             (set (possible-moves-piece (piece-at board 7 1)
                                        board))))
      (is (= (set [{:move [[7 6] [6 4]]}
                   {:move [[7 6] [5 5]]}
                   {:move [[7 6] [5 7]]}])
             (set (possible-moves-piece (piece-at board 7 6)
                                        board))))
      (is (= (set [{:move [[0 6] [1 4]]}
                   {:move [[0 6] [2 5]]}
                   {:move [[0 6] [2 7]]}])
             (set (possible-moves-piece (piece-at board 0 6)
                                        board)))))))
