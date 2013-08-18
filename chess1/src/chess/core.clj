(ns chess.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn file
  "Whether a character denotes a file (think 'column') on the chess board. Files
 range from lower case 'a' (white player's leftmost) to 'h' (white player's
 rightmost).

   Returns the index of the rank when the board is a seq of seqs."
  [ch]
  (let [i (.indexOf "abcdefgh" (str ch))]
    (if (>= i 0)
      i)))

(defn rank
  "Whether a character denotes a rank (think 'row') on the chess board.
Ranks range from '1' (closest to white player) to '8' (furthest from white
player.)

   Returns the char itself or nil if it's not a rank."
  [ch]
  (let [i (.indexOf "12345678" (str ch))]
    (if (>= i 0)
      (- 7 i))))

(defn parse-square
  "Parses the coordinates for a square into a seq [file rank], suitable for
  indexing the board."
  [s]
  [(file (.charAt s 0))
   (rank (.charAt s 1))])

(defn piece-at
  "The piece at a rank and file on the board.

   The order of the parameters is (file,rank) as per the Standard Algebraic Notation, for instance, E8.
   Rank and file should be integers in the range [0..7]."
  ([board ^String square]
     (let [f (file (.charAt square 0))
           r (rank (.charAt square 1))]
       (piece-at board f r)))
  ([board file rank]
     (-> board
         (nth rank)
         (nth file))))

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

   The board is a 2D seq where an entry (i,j) contains the piece in cell (i,j)
   starting furthest from the white player's position."
  [xs]
  (let [piece (fn [ch]
                (if-let [kind (pieces (Character/toUpperCase ch))]
                  {:kind  kind
                   :color (if (Character/isUpperCase ch)
                            :white
                            :black)
                   :str   ch        ; string representation of this piece.
                   }))]
    (map #(map piece %)
         xs)))

(defn print-board
  "Prints to stdout the board as seen by the white player."
  [board]
  (doseq [rank board]
    (println (str/join (map (fn [piece]
                              (if (nil? piece)
                                "."
                                (:str piece)))
                            rank)))))

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

(defn square-check?
  "Whether a square is in check by a piece of informed player."
  [board player sq]
  ;; TODO - Finish me.
  

  
  )

(defn- new-game
  "Creates a new match of chess.

   A game object holds information about the game, such as the current player
   about to move and the state of the chess board."
  []
  {:board board
   :current-player :white
   :king-moved     #{}
   :rook-moved     #{}})

(defn- prompt-move
  "Prints a message to stdout asking for the player's move, reads and returns a
   line of input from stdin."
  [player]
  (println (str player " player, your move ?"))
  (.readLine *in*))

(defn parse-move
  "Parses a move command according to the Standard Algebraic Notation or coordinate notation

   Example of valid moves:
   e4
   Bc6
   Nxf6
   O-O
   e2g4"
  [s]
  (let [re-pawn      #"([a-h])([1-8])"
        re-pawn-cap  #"([a-h])x([a-h])([1-8])(\(ep\))?"
        re-piece     #"([BKNR])([a-h]?)(x?)([a-h])([1-8])"
        re-coord     #"([a-h])([1-8])([a-h])([1-8])"]
    (cond
     ;; pawn moves
     (re-matches re-pawn s)
     (let [[_ f r] (first (re-seq re-pawn s))]
       {:piece :pawn
        :to [(file (.charAt f 0))
             (rank (.charAt r 0))]})

     ;; pawn captures
     (re-matches re-pawn-cap s)
     (let [[_ from-file f r ep?] (first (re-seq re-pawn-cap s))]
       {:piece :pawn
        :to [(file (.charAt f 0))
             (rank (.charAt r 0))]
        :ep  (boolean ep?)
        :cap true
        :from-file (if from-file (file (.charAt from-file 0)))})

     ;; piece moves or captures
     (re-matches re-piece s)
     (let [[_ p from-file cap? f r] (first (re-seq re-piece s))]
       {:piece (pieces p)
        :to [(file (.charAt f 0))
             (rank (.charAt r 0))]
        :cap (boolean cap?)
        :from-file (if from-file (file (.charAt from-file 0)))})

     ;; coordinate notation
     (re-matches re-coord s)
     (let [[_ f1 r1 f2 r2] (first (re-seq re-coord s))]
       {:from [(file (.charAt f1 0))
               (rank (.charAt r1 0))]
        :to [(file (.charAt f2 0))
             (rank (.charAt r2 0))]})

     ;; castle with king's rook
     (= "0-0" s)
     {:piece  :king
      :castle :king}

     ;; castle with queen's rook
     (= "0-0-0" s)
     {:piece  :king
      :castle :queen}

     ;; default
     :else
     {:invalid     true
      :parse-error true
      :input       s})))


(defn- move
  "Updates the game with the move performed by the player."
;  [game mv]
                                        ;  (let [b (:board game)])
  ; FIXME: finish me
  []
  )

(defn invalid-castle?
  "Whether the move is a castle which violates the castling rules.

   Returns a string explaining the violated castling rule,
   or nil if the castling is valid."
  [mv game]
  (let [board (:board game)
        player (:current-player game)
        other (other-player player)
        castle (:castle mv)]
    (cond
     (contains? (:king-moved game)
                player)
     "King moved earlier in the game."

     (contains? (:rook-moved game)
                [player (:castle mv)])
     "Rook moved earlier in the game."

     (let [sq (case player
                   :white "e1"
                   :black "e8")]
       (square-check? board
                      other-player
                      sq))
     "The king is in check."

     (let [sq (case [player castle]
                [:white :king]  "g1"
                [:white :queen] "c1"
                [:black :king]  "g8"
                [:black :queen] "c8")]
       (square-check? board
                      other-player
                      sq))
     "The king would be in check after castling."
     
     (let [sqs (case [player castle]    ; squares between king and rook.
                 [:white :king]  ["f1" "g1"]
                 [:white :queen] ["b1" "c1" "d1"]
                 [:black :king]  ["f8" "g8"]
                 [:black :queen] ["b8" "c8" "d8"])]
       (some (fn [sq]
               (piece-at board sq))
             sqs))
     "There are pieces between the king and rook."

     (let [sq (case [player castle]
                   [:white :king]  "f1"
                   [:white :queen] "d1"
                   [:black :king]  "f8"
                   [:black :queen] "d8")]
       (square-check? board other-player sq))
     "The king moves through a square that is attacked by a piece of the opponent.")))

(defn eval-move
  "Evaluates a move on the current game."
  [mv game]
  (cond
   ;; Command could not be parsed, return as is.
   (:invalid mv)
   mv

   (:castle mv)
   (if-let [invalid (invalid-castle? mv game)]
     {:invalid true
      :cause (str "Invalid castle: " invalid)})))

(defn -main
  "Starts a game of chess, alternately asking for players' moves on the command
  line."
  ; FIXME: finish me
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (loop [game (new-game)]
    (when-not (:over game)
      (print-board (:board game))
      (let [str-mv (prompt-move (:current-player game))
            mv     (eval-move (parse-move str-mv)
                              game)]
        (if (:invalid mv)
          (if (:parse-error mv)
            (do (println (str "Cannot parse move: " (:input mv)))
                (recur game))

            (do (println (str "Invalid move: " (:cause mv)))
                (recur game)))

          (recur (move game mv)))))))
