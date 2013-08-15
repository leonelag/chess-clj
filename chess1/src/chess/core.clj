(ns chess.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def pieces
  {\K :king
   \Q :queen
   \N :knight
   \B :bishop
   \R :rook
   \P :pawn})

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

(defn- print-board
  "Prints to stdout the board as seen by the white player."
  [board]
  (doseq [file board]
    (println (str/join (map (fn [piece]
                              (if (nil? piece)
                                "."
                                (:str piece)))
                            file)))))

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

(defn- new-game
  "Creates a new match of chess.

   A game object holds information about the game, such as the current player
   about to move and the state of the chess board."
  []
  {:board board
   :current-player :white})

(defn- prompt-move
  "Prints a message to stdout asking for the player's move, reads and returns a
   line of input from stdin."
  [player]
  (println (str player " player, your move ?"))
  (.readLine *in*))

(defn- parse-move
  ""
  [s]
  )

(defn- move
  "Updates the game with the move performed by the player."
;  [game mv]
;  (let [b (:board game)])
  []
  )

(defn eval-move [])

(defn -main
  "Starts a game of chess, alternately asking for players' moves on the command
  line."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (loop [game (new-game)]
    (when-not (:over game)
      (print-board (:board game))
      (let [str-mv (prompt-move (:current-player game))
            mv     (eval-move str-mv)]



        (if (:illegal mv)
          (do (println (str "Illegal move: " (:cause mv)))
              (recur game))
          (recur (move game mv)))))))
