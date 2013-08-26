(ns chess.core
  (:gen-class)
  (:require [clojure.string :as string]))

;; TODO - remove this
(def debug println)

;; Utility functions
(def not-nil? (complement nil?))

(defn file
  "Converts a file character (think 'column') to an int index on the chess board.

   Files range from lower case 'a' (white player's leftmost) to 'h' (white
   player's rightmost)."
  [ch]
  (let [i (.indexOf "abcdefgh" (string/lower-case (str ch)))]
    (if (>= i 0)
      i)))

(defn rank
  "Converts a rank character (think 'row') to an int index on the chess board.

   Ranks range from '1' (closest to white player) to '8' (furthest from white
   player.)"
  [ch]
  (let [i (.indexOf "12345678" (str ch))]
    (if (>= i 0)
      i)))

(defn to-std-notation
  "Converts internal data structure coordinates (row,col) into human-readable
  rank/file notation."
  [row col]
  (let [ranks "ABCDEFGH"
        files "12345678"]
    (str (.charAt ranks col)
         (.charAt files row))))

(defn parse-square
  "Parses the coordinates for a square into a seq [file rank], suitable for
  indexing the board."
  [s]
  [(rank (.charAt s 1))
   (file (.charAt s 0))])

(defn piece-at
  "The piece at a rank and file on the board.

  Takes either a string in the Standard Algebraic Notation, for instance, E8
or a pair (row,col) where row and col are in the range [0..7]
and are indexes into the board data structure."
  ([board ^String square]
     (let [[row col] (parse-square square)]
       (piece-at board row col)))
  ([board row col]
     (get-in board [row col])))

(def pieces
  {\K :king
   \Q :queen
   \N :knight
   \B :bishop
   \R :rook
   \P :pawn})

(defn other-player
  "The color of the adversary of the parameter player."
  [player]
  (case player
    :white :black
    :black :white))

(defn parse-board
  "Parse a board from a seq of strings.

   Strings are in the chess notation format. Each character should be one of
   K,Q,N,B,R,P for King, Queen, kNight, Bishop, Rook, Pawn. Any other letter is
   considered an empty cell.

   Upper case characters for white pieces, lower case for black pieces.

   The data structure of the board is a 2D seq where an entry (i,j) contains the
   piece in cell (i,j) starting closest to the white player's position."
  [xs]
  (vec (reverse
    (for [[row rownum] (map vector xs (range 7 -1 -1))]
      (vec (for [[ch colnum] (map vector row (range 8))]
             (if-let [kind (pieces (Character/toUpperCase ch))]
               {:kind  kind
                :color (if (Character/isUpperCase ch)
                         :white
                         :black)
                :str   ch        ; string representation of this piece.
                :row   rownum
                :col   colnum})))))))

(defn print-board
  "Prints to stdout the board as seen by the white player."
  [board]
  (doseq [[rank, rownum] (map vector
                              (reverse board)
                              (range 8 0 -1))]
    (println (str rownum
                  " |"
                  (string/join (map (fn [piece]
                                      (if (nil? piece)
                                        "."
                                        (:str piece)))
                                    rank)))))
  (println "   --------")
  (println "   abcdefgh"))

(defn print-game
  "Prints current state of the game.

  Prints the board and announces checks, wins and losses."
  [game]
  (print-board (:board game))
  (if (:check game)
    (if-let [winner (:checkmate game)]
      (println "Checkmate ! "
               (case winner
                 :white "White"
                 :black "Black")
               " wins.")
      (println "Check.")))
  (println))

(def board
  "The state of a chess board at the beginning of a match."
  (parse-board ["rnbqkbnr"
                "pppppppp"
                "........"
                "........"
                "........"
                "........"
                "PPPPPPPP"
                "RNBQKBNR"]))

(defn within-board
  "Whether coordinates for a square are in the board bounds."
  [[row col]]
  (and (<= 0 row 7)
       (<= 0 col 7)))

(defn player-pieces
  "A seq of the pieces of a player on the board."
  [board player]
  (let [pieces (flatten board)
        idxs   (for [r (range 8), c (range 8)]
                 [r c])]
    (->> (map (fn [p [r c]]
                (if p
                  (assoc p, :row r, :col c)))
              pieces
              idxs)
         (filter (fn [p]
                   (and (not-nil? p)
                        (= player (:color p))))))))

(defn knight-squares
  "The squares that a knight at (row,col) can move to."
  [row col]
  (let [deltas [[1 2]   [2 1]   [2 -1] [1 -2]
                [-1 -2] [-2 -1] [-2 1] [-1 2]]]
    (filter within-board
            (for [[dr dc] deltas]
              [(+ row dr) (+ col dc)]))))

(defn diagonals
  "The diagonals starting from a square.
   Returns seqs of seqs of squares, each containing the squares in a direction: NE, NW, SW, SE.
   None of the inner seqs is empty."
  [row col]
  (->>
   (for [dr [-1 1], dc [-1 1]]
     (take-while
      within-board
      (drop 1
            (iterate (fn [[r c]] [(+ r dr) (+ c dc)])
                     [row col]))))
   (filter (complement empty?))))

(defn parallels
  "Squares in the same row or same col, starting from a square.
   Returns seqs of seqs of squares, each containing the squares in a direction: N, W, S, E.
   None of the inner seqs is empty."
  [row col]
  (->>
   (for [[dr dc] [[0 1], [1 0], [0 -1], [-1 0]]]
     (take-while
      within-board
      (drop 1 (iterate (fn [[r c]] [(+ r dr) (+ c dc)])
                       [row col]))))
   (filter (complement empty?))))

(defn around
  "The squares around a square."
  [row col]
  (->>
   (map (fn [[dr dc]]
          [(+ row dr) (+ col dc)])
        [[0 1], [1 1], [1 0], [1 -1], [0 -1], [-1 -1], [-1 0], [-1 1]])
   (filter within-board)))

(defn square-attacked?
  "Whether a square is in check by a piece of informed player."
  [board player [row col]]
  ;; checks over the square to find a piece is of the same color and kind as param
  (let [is-piece-at? (fn [kinds sqs]
                       (some (fn [[r c]]
                               (if-let [piece (piece-at board r c)]
                                 (and (= player (:color piece))
                                      (kinds (:kind piece)))))
                             sqs))]
    (or
     ;; square in check by a pawn
     (let [pawn-row (case player
                      :white (dec row)
                      :black (inc row))
           sqs (filter within-board [[pawn-row (dec col)]
                                     [pawn-row (inc col)]])]
       (is-piece-at? #{:pawn} sqs))

     ;; square in check by a knight
     (is-piece-at? #{:knight}
                   (knight-squares row col))

     ;; look in diagonals for the first piece, see if it's queen or bishop
     (->> (diagonals row col)                   ; in each diagonal
          (map #(first (drop-while              ; get the first piece
                        (fn [[r c]] (nil? (piece-at board r c)))
                        %)))
          (filter not-nil?)                     ; some diagonals do not have pieces
          (is-piece-at? #{:queen :bishop}))

     (->> (parallels row col)                   ; in each row and col
          (map #(first (drop-while              ; get the first piece
                        (fn [[r c]] (nil? (piece-at board r c)))
                        %)))
          (filter not-nil?)                     ; some do not have pieces
          (is-piece-at? #{:queen :rook}))

     ;; square in check by king
     (is-piece-at? #{:king}
                   (around row col)))))

(defn- new-game
  "Creates a new match of chess.

   A game object holds information about the game, such as the current player
   about to move and the state of the chess board."
  []
  {:board board
   :current-player :white
   :king-moved     #{}
   :rook-moved     #{}
   :check          nil
   :checkmate      nil})

(defn- prompt-move
  "Prints a message to stdout asking for the player's move, reads and returns a
   line of input from stdin."
  [player]
  (let [name (case player
               :white "White"
               :black "Black")]
    (println name "player, your move ?"))
  (.readLine *in*))

(defn pawn-to
  "Attempt to move a pawn of color (player) to (row,col)."
  [board player row col]
  (let [is-pawn? (fn [r]
                   (if-let [p (piece-at board r col)]
                     (if (and (= player (:color p))
                              (= :pawn  (:kind p)))
                       r)))
        invalid {:invalid :true,
                 :cause (str "no pawn can move to " (to-std-notation row col))}]
    (if (nil? (piece-at board row col))
      ;; Move to free square. Look for a pawn that can move to this square
      (if-let [from-row (cond
                         (and (= :white player) (= 3 row))
                         (cond (is-pawn? (dec row))
                               (dec row)

                               (and (is-pawn? (dec (dec row)))
                                    (nil? (piece-at board (dec row) col)))
                               (dec (dec row)))

                         (= :white player)
                         (is-pawn? (dec row))

                         (and (= :black player) (= 4 row))
                         (cond (is-pawn? (inc row))
                               (inc row)

                               (and (is-pawn? (inc (inc row)))
                                    (nil? (piece-at board (inc row) col)))
                               (inc (inc row)))

                         (= :black player)
                         (is-pawn? (inc row)))]
        {:move [[from-row col] [row col]]}
        invalid)

      ;; square occupied
      invalid)))

(defn pawn-capture
  "Evalutes a capture by pawn.

   Returns :invalid or :move according to documentation of parse-eval-move"
  ;; TODO Consider en-passant capture
  [board player row col from-col ep?]
  (let [from-row (case player
                   :white (dec row)
                   :black (inc row))
        p (piece-at board row col)
        ;; a square may be captured by a pawn on its west or east
        [west east] (map (fn [c]
                           (if-let [p (and (within-board [from-row c])
                                           (piece-at board from-row c))]
                             (if (and (= player (:color p))
                                      (= :pawn (:kind p)))
                               p)))
                         [(dec col) (inc col)])]
    (if (or
         ;; attempt to capture empty square
         (nil? p)

         ;; attempt to capture own piece
         (= player (:color p))

         ;; two pawns can capture but hint does not disambiguate
         (and (and west east)
              (not= from-col (:col west))
              (not= from-col (:col east)))

         ;; one pawn can capture but hint does not point at it
         (and west
              (not= from-col (:col west)))
         (and east
              (not= from-col (:col east)))

         ;; no pawn can capture
         (not (or west east)))
      {:invalid true,
       :cause (str "no pawn can capture at " (to-std-notation row col))}

      {:move [[from-row (cond
                         from-col from-col
                         west     (:col west)
                         east     (:col east))]
              [row col]]})))

(defn path
  "The sequence of squares between two squares."
  [[r1 c1] [r2 c2]]
  (let [sgn (fn [n]
              (cond
               (< 0 n) 1
               (> 0 n) -1
               :else 0))
        dr (sgn (- r2 r1)),
        dc (sgn (- c2 c1))]
    (take-while
     (fn [[r c]]
       (and (not= r r2)
            (not= c c2)))
     (drop 1
           (iterate (fn [[r c]]
                      [(+ r dr) (+ c dc)])
                    [r1 c1])))))

(defn piece-between?
  "Whether there is a piece in the path between two squares"
  [board [r1 c1] [r2 c2]]
  (some (fn [[r c]]
          (not-nil? (piece-at board r c)))
        (path [r1 c1] [r2 c2])))

(defn piece-move
  "Validates a piece move."
  [board player row col kind from-file cap?]
  (let [p (piece-at board row col)]
    (cond
     (and cap? (nil? p))
     {:invalid true, :cause "Cannot capture empty square"}

     (= player (:color p))
     {:invalid true, :cause "Cannot capture own piece"}

     ;; find the pieces that can move into the square
     :else
     (let [sqs (case kind
                 :knight (knight-squares row col)
                 :bishop (apply concat (diagonals row col))
                 :rook   (apply concat (parallels row col))
                 :queen  (mapcat concat
                                 (diagonals row col)
                                 (parallels row col))
                 :kind   (around row col))
           ps (->> sqs
                   (map (fn [[r c]]
                          (piece-at board r c)))
                   (filter (fn [p]
                             (and (not-nil? p)
                                  (= kind (:kind p))
                                  (= player (:color p))
                                  (or
                                   (= kind :knight)
                                   (not (piece-between? board
                                                        [(:row p) (:col p)]
                                                        [row col])))))))]
       (cond
        (> (count ps) 1)
        {:invalid true, :cause "Ambiguous move."}

        (< (count ps) 1)
        {:invalid true, :cause "No piece can make this move."}

        :else
        (let [p (first ps)]
          {:move [[(:row p) (:col p)] [row col]]}))))))

(defn invalid-castle?
  "Whether the move is a castle which violates the castling rules.

   Returns a string explaining the violated castling rule,
   or nil if the castling is valid."
  [side game]
  (let [board (:board game)
        player (:current-player game)
        other (other-player player)]
    (cond
     (contains? (:king-moved game)
                player)
     "King moved earlier in the game."

     (contains? (:rook-moved game)
                [player side])
     "Rook moved earlier in the game."

     (let [[row col] (parse-square (case player
                                     :white "e1"
                                     :black "e8"))]
       (square-attacked? board
                      other-player
                      row col))
     "The king is in check."

     (let [[row col] (parse-square (case [player side]
                                     [:white :king]  "g1"
                                     [:white :queen] "c1"
                                     [:black :king]  "g8"
                                     [:black :queen] "c8"))]
       (square-attacked? board
                      other-player
                      row col))
     "The king would be in check after castling."

     (let [sqs (case [player side]    ; squares between king and rook.
                 [:white :king]  ["f1" "g1"]
                 [:white :queen] ["b1" "c1" "d1"]
                 [:black :king]  ["f8" "g8"]
                 [:black :queen] ["b8" "c8" "d8"])]
       (some (fn [sq]
               (piece-at board sq))
             sqs))
     "There are pieces between the king and rook."

     (let [[row col] (parse-square (case [player side]
                                     [:white :king]  "f1"
                                     [:white :queen] "d1"
                                     [:black :king]  "f8"
                                     [:black :queen] "d8"))]
       (square-attacked? board other-player row col))
     "The king moves through a square that is attacked by a piece of the opponent."


     ;; Happens without triggering previous checks when a player loses its rook.
     ;; So his king and rook never moved, but castling is impossible, because
     ;; the pieces at those squares are not the good ones.
     (let [[k r] (map (fn [[r c]] (piece-at board r c))
                      (case [player side]
                        [:white :king]  ["e1" "h1"]
                        [:white :queen] ["e1" "a1"]
                        [:black :king]  ["e8" "h8"]
                        [:black :queen] ["e8" "a8"]))]
       (or
        (not= [:king player] (select-keys [:kind :color] k))
        (not= [:rook player] (select-keys [:kind :color] r))))
     "Cannot castle with these pieces.")))

(defn castle
  [side game]
  (if-let [cause (invalid-castle? side game)]
    {:invalid true, :cause cause}

    (let [board (:board game)
          player (:current-player game)
          king (case player
                 :white (piece-at board 0 4)
                 :black (piece-at board 7 4))
          [rook [kr kc] [rr rc]]
          (case [player side]
            [:white :king]  [(piece-at board 0 7) [0 6] [0 5]]
            [:white :queen] [(piece-at board 0 0) [0 2] [0 3]]
            [:black :king]  [(piece-at board 7 7) [7 6] [7 5]]
            [:black :queen] [(piece-at board 7 0) [7 2] [7 3]])]
      {:remove [[(:row king) (:col king)]
                [(:row rook) (:col rook)]]
       :add    [[kr kc (assoc king :row kr :col kc)]
                [rr rc (assoc rook :row rr :col rc)]]})))

(defn checkmate?
  "Verifies is a game is in check mate."
  ;; TODO - Finish me.
  [board player]
  false)

(defn check?
  "Verifies that a player's king is in check."
  ;; TODO - Finish me.
  [board player]
  (let [king (first (filter (fn [p]
                              (= :king (:kind p)))
                            (player-pieces board player)))]
    (square-attacked? board
                      (other-player player)
                      [(:row king)
                       (:col king)])))

(defn board-move
  "Updates the board."
  [board mv]
  (letfn
      [(remove-at [board removes]
         (reduce (fn [b [r c]]
                   (update-in b [r c] (fn [_] nil)))
                 board
                 removes))
       (add-at [board adds]
         (reduce (fn [b [r c p]]
                   (update-in b [r c] (fn [_] p)))
                 board
                 adds))

       (move [board mv]
         (if-let [[[r1 c1] [r2 c2]] mv]
           (let [p (assoc (piece-at board r1 c1)
                     :row r2
                     :col c2)]
             (-> board
                 (remove-at [[r1 c1]])
                 (add-at    [[r2 c2 p]])))
           board))]
    (-> board
        (remove-at (:remove mv))
        (add-at    (:add mv))
        (move      (:move mv)))))

(defn parse-move
  "Parses a move command according to the Standard Algebraic Notation or coordinate notation

   Example of valid moves:
   e4
   Bc6
   Nxf6
   O-O
   e2g4"
  [s game]
  (let [re-pawn      #"([a-h][1-8])"
        re-pawn-cap  #"([a-h])x([a-h][1-8])(\(ep\))?"
        re-piece     #"([BKNRQ])([a-h]?)(x?)([a-h][1-8])"
        re-coord     #"([a-h][1-8])([a-h][1-8])"
        board  (:board game)
        player (:current-player game)

        ;; Converts the empty strings returned by re-seq into nil.
        not-empty?   (fn [s] (if (not (empty? s))
                               s))]
    (cond
     ;; pawn moves
     (re-matches re-pawn s)
     (let [[_ sq] (first (re-seq re-pawn s))
           [row, col] (parse-square sq)]
       (pawn-to board player row col))

     ;; pawn captures
     (re-matches re-pawn-cap s)
     (let [[_ from-file sq ep?] (first (re-seq re-pawn-cap s))
           from-col (if from-file (file (.charAt from-file 0)))
           [row, col] (parse-square sq)]
       (pawn-capture board player row col from-col ep?))

     ;; piece moves or captures
     (re-matches re-piece s)
     (let [[_ kind from-file cap? sq] (first (re-seq re-piece s))
           [row,col] (parse-square sq)]
       (piece-move board player row col (pieces (.charAt kind 0))
                   (not-empty? from-file)
                   (not-empty? cap?)))

     ;; coordinate notation
     (re-matches re-coord s)
     ;; TODO - Finish me
     (throw (IllegalArgumentException. "Not implemented."))

     ;; castle with king's rook
     (= "0-0" s)
     (castle game :king)

     ;; castle with queen's rook
     (= "0-0-0" s)
     (castle game :queen)

     ;; default
     :else
     {:invalid     true
      :parse-error true
      :input       s})))

(defn parse-eval-move
  "Parses and evaluates a move.

   The result contains the keys:
   :invalid - if the move was invalid
   :move    - [[row-from,col-from], [row-to,col-to]] if a piece is to be moved
   :remove  - seq of [row,col] with the coordinates of the pieces to be removed
   :add     - seq of [row,col,piece] with the coordinates of pieces to add."
  [s game]
  (let [mv (parse-move s game)
        board (:board game)
        player (:current-player game)]
    (cond
     (:invalid mv)
     mv

     ;; a move is only valid if it does not leave player in check
     (check? (board-move board mv) player)
     {:invalid true, :cause (str player " is in check.")}

     :else    ; valid move
     mv)))

(defn- move
  "Updates the game state after a player moved."
  [game mv]
  (letfn [(king-moved? [board mv]
            (if-let [[[r c] _] (:move mv)]
              ;; the first moves of the kings are E1 and E8
              (case [r c]
                [0 4] :white
                [7 4] :black
                false)))
          (rook-moved? [board mv]
            (if-let [[[r c] _] (:move mv)]
              (case [r c]
                [0 0] [:white :queen]
                [0 7] [:white :king]
                [7 0] [:black :queen]
                [7 7] [:black :king]
                false)))
          (update-if [m key val]
            ;; updates m with val only if val is true; else returns m untouched.
            (if val
              (update-in m key #(conj % val))
              m))]

    ;; TODO - Verify en passant
    ;; TODO - Verify king in check
    ;; TODO - Verify checkmate.
    (let [board (:board game)
          b (board-move board mv)
          other-player (other-player (:current-player game))]
      (-> game
          (assoc
            :board b
            :current-player other-player
            :check      (if (check? board other-player)
                          other-player)
            :checkmate  (checkmate? board other-player))
          (update-if :king-moved (king-moved? board mv))
          (update-if :rook-moved (rook-moved? board mv))))))

(defn -main
  "Starts a game of chess, alternately asking for players' moves on the command
  line."
  ; FIXME: finish me
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (loop [game (new-game)]
    (when-not (:over game)
      (print-game game)
      (let [str-mv (prompt-move (:current-player game))]
        (case str-mv
          "quit"
          ; end game, no recur.
          (println "Goodbye !")

          "undo"
          (do (println "undo (not implemented)")
              (recur game))

          "history"
          (do (println "history (not implemented)")
              (recur game))

          ;; Not a command. Parse as a game move.
          (let [mv (parse-eval-move str-mv game)]
            (if (:invalid mv)
              (if (:parse-error mv)
                ;; parse error
                (do (println (str "Cannot parse move: " (:input mv)))
                    (recur game))

                ;; move is invalid according to rules.
                ;; Example: try to move a bishop sideways
                (do (println (str "Invalid move: " (:cause mv)))
                    (recur game)))

              (recur (move game mv)))))))))
