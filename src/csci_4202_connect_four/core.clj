(ns csci-4202-connect-four.core
  (:gen-class)
  (:require [cheshire.core :refer :all]
            [clojure.core.matrix :refer :all])
  (:import (java.io BufferedReader BufferedWriter)
           (java.lang.System)))


(declare min-r)
(declare max-r)

(defn receive-game-string []
  (parse-string (read-line)))

;; Define the max depth to which the program will calculate moves.
;; Depth starts at 0, so a depth of 4 will traverse 5 levels.
(def MAXDEPTH 4)
;; Define the weights for each "group" of pieces.
;; 4 in a row = 100 points
;; 3 in a row = 10 points
;; 2 in a row = 1 point
;; 1 piece = 0 points
(def move-values {4 100 3 10 2 1 1 0})

;;; apply-move
;; Function to apply a move represented as a column number from 0 - (size-1)
;; for a given player.
;; Example:
;; 0-0-0-0-0-0-0
;; 0-0-0-0-0-0-0
;; 0-0-2-0-0-0-0
;; 2-0-2-0-0-0-0
;; 1-0-1-1-0-0-0
;; In this example, (apply-move <state> 1 1) will apply the winning move for player 1.
(defn apply-move
  [state move player]
  ;; Bind column to the column selected by 'move'
  (let [column (get state move)]
    ;; If the first entry in the column is non-zero, then return nil because this column is full.
    (if-not (= (first column) 0)
      nil
      ;; If the last element in the column is zero, immediately insert at the end.
      (if (= (peek column) 0)
        ;; Associate the value at [move column.size-1] with the player's number (represents their move)
        (assoc-in state [move (- (count column) 1)] player)
        ;; Loop with initial index binding of 1 (We already checked zero in the first if-statement.
        (loop [index 1]
          ;; If this value is zero, recursively call with ++index.
          (if (= (get column index) 0)
            ;; If true recursive call.
            (recur (inc index))
            ;; If false, stop here and associate the given position with the player's number (move).
            (assoc-in state [move (- index 1)] player)

            ))))))

;;; get-valid-moves
;; Returns a map of valid moves (successor board states) for a given player in the form:
;; {<index> <state>
;;  <index> <state>
;;  <index> <state>}
;; where <state> is the result of calling (apply-move <old-state> <index> <player>)
(defn get-valid-moves [state player]
  ;; Put results into a map so we can access a move by its index value.
  (into {}
        ;; Return a collection of key-value pairs, where the value is the successor move, and the
        ;; key is the move index.
        (map-indexed
          (fn [keyv val] [keyv val])
          ;; Map each possible move index to its resulting board state.
          (map #(apply-move state % player) (range (count state)))
          )
        )
  )

;;; switch-player
;; Returns the opposing player for the current player.
(defn switch-player [player]
  (if (= player 1)
    2
    1
    )
  )

;;; get-column-fours
;; Accepts a board state, and returns a list of all pairs
;; of four consecutive spaces for each row in state.
(defn get-column-fours [state]
  (apply concat
         (map #(partition 4 1 %) state))
  )

;;; get-row-fours
;; Identical to get-column-fours, except it operates on the transpose
;; of the given state. This means we return a list of all pairs of
;; four consecutive spaces for each column in a state.
(defn get-row-fours [state]
  (apply concat
         (map #(partition 4 1 %) (transpose state)))
  )

;;; get-diagonal-fours
;; Returns a list of all pairs of 4 consecutive spaces for both diagonal
;; directions
(defn get-diagonal-fours [state]
  (let [vec [] rev (reverse state)]
    ;; Collapse this list one level to get a list of all possible groups of four in
    ;; every diagonal.
    (apply concat
           ;; Partition each diagonal into all possible groups of 4 sequential spaces.
           (map #(partition 4 1 %)
                (filter
                  ;; Some diagonals are less than 4 in length. We can't use a hardcoded
                  ;; solution here because we need to support all board sizes.
                  #(>= (count %) 4)
                  ;; Collapse the list one level so we have a list of all diagonals.
                  (apply concat
                         ;; Iterate through all possible diagonals of the matrix, and both the
                         ;; left facing and right facing diagonal to vector 'vec'
                         (for [x (range (- 1 (count state)) (count (first state)))]
                           (conj vec (diagonal state x) (diagonal rev x))
                           )
                         )
                  )
                )
           )
    )
  )

;;; points-per-four
;;  Calculates the number of points a given state is worth for a player.
;;
;;  States that contains pieces from both players return no points,
;;  because these states will never cause an endgame.
;;
;;  States with moves from the current player only will return the number of
;;  points specified in move-values.
;;
;;  States with moves from the opponent player only will return the number of
;;  points specified in move-values multiplied by -1.
(defn points-per-four [group player]
  (let [fmap (frequencies group) opponent (switch-player player)]
    (if (and (contains? fmap player) (not (contains? fmap opponent)))
      (get move-values (get fmap player))
      (if (and (contains? fmap opponent) (not (contains? fmap player)))
        (* -1 (get move-values (get fmap opponent)))
        0
        )
      )
    )
  )
;;; utility
;;  Returns the utility of a given state. The utility of a state is equal
;;  to the sum of the points earned (or lost) in each column, row, and diagonal.
(defn utility [state player]
  (apply + (map #(points-per-four % player) (concat (get-column-fours state) (get-row-fours state) (get-diagonal-fours state))))
  )

;;; check-diag
;;  Returns true if a game-ending move exists in any diagonal.
(defn check-diag [state]
  (some true? (map #(when (not= (first %) 0) (apply = %))
                   (get-diagonal-fours state)
                   )
        )
  )
;;; check-column
;;  Returns true if a game-ending move exists in any column.
(defn check-column [state]
  (some true?
        (map #(when (not= (first %) 0) (apply = %))
             (get-column-fours state))
        )
  )
;;; check-row
;;  Returns true if a game-ending move exists in any row.
(defn check-row [state]
  (some true?
        (map #(when (not= (first %) 0) (apply = %))
             (get-row-fours state))
        )
  )
;;; end-game
;;  Returns true if a game-ending move exists in any
;;  row, column, or diagonal.
(defn end-game [state]
  (or
    (check-row state)
    (check-column state)
    (check-diag state)
    )
  )
;;; random-move
;;  Returns a random valid move.
;;  Currently unused (was used for debugging).
(defn random-move [state player]
  (when (not (end-game state))
    (loop [states (get-valid-moves state player) num (rand-int (count states))]
      (if-not (nil? (get states num))
        num
        (recur (dissoc states num) (rand-int (count states)))
        )
      )
    )
  )
;;; min-r
;;  Returns a utility state if the game is at the MAXDEPTH,
;;  if the current state is nil, or if the current state
;;  is an end-game state.
;;  Otherwise, calls max-r recursively with a list of the current
;;  moves at this state.
(defn min-r [state alpha beta depth curr-player player]
  (if (or (nil? state) (= depth MAXDEPTH) (end-game state))
    ;; If max depth, return the utility of this state.
    (utility state player)
    ;; Bind the map of valid moves to state-map.
    ;; Bind size to the size of the state-map.
    (let [state-map (get-valid-moves state curr-player) size (count state-map)]
      ;; Start loop with v=+inf.
      (loop [v 99999999 b-val beta current 0]
        (let [
              ;; Bind state to the state at the current index.
              state (get state-map current)
              ;; Bind x to the minimum of the current v value and the result of max-r
              x (min v (max-r state alpha b-val (inc depth) (switch-player curr-player) player))
              ]
          ;; If max can make a better move than this move, stop here and prune the branch
          (if (<= x alpha)
            (do
              (binding [*out* *err*]
                ;;(println (str "Pruned at Depth: " depth))
                )
              x)
            ;; If we've hit the end of the list of moves, return the minimum move in x.
            (if (= (dec size) current)
              (do
                (binding [*out* *err*]
                  ;;(println (str "All moves explored at depth: " depth " min value: " newbeta))
                  )
                x
                )
              ;; Otherwise, recursively loop, and pass x along with a (possibly) new value for beta.
              (do
                (binding [*out* *err*]
                  ;;(println (str "Depth: " depth " Current min at Option " current ": " x))
                  )
                (recur x (min x beta) (inc current))
                )
              )

            )
          )
        )
      )
    )
  )
;;; max-r
;;  Returns a utility state if the game is at the MAXDEPTH,
;;  if the current state is nil, or if the current state
;;  is an end-game state.
;;  Otherwise, calls min-r recursively with a list of the current
;;  moves at this state.
(defn max-r [state alpha beta depth curr-player player]
  (if (or (nil? state) (= depth MAXDEPTH) (end-game state))
    ;; If max depth, return the utility of this state.
    (utility state player)
    (let [state-map (get-valid-moves state curr-player) size (count state-map)]
      ;; Start loop with v=-inf
      (loop [v -99999999 a-val alpha current 0]
        (let [
              ;; Bind state to the state at the current index.
              state (get state-map current)
              ;; Bind x to the maximum of v and the result of min-r
              x (max v (min-r state a-val beta (inc depth) (switch-player curr-player) player))
              ]
          (if (<= beta x)
            ;; If the opponent can make a better move (smaller value)
            ;; than this one, stop here to prune.
            (do
              (binding [*out* *err*]
                ;;(println (str "Pruned at Depth: " depth))
                )
              x
              )
            ;; If we've hit the end of the list of moves, return the best move in x
            (if (= (dec size) current)
              (do
                (binding [*out* *err*]
                  ;;(println (str "All moves explored at depth: " depth " max value: " newalpha))
                  )
                x
                )
              ;; Otherwise, recursively call with x and a (possibly) new alpha value.
              (do
                (binding [*out* *err*]
                 ;; (println (str "Depth: " depth " Current max at Option " current ": " x))
                  )
                (recur x (max x alpha) (inc current))
                )
              )
            )
          )

        )

      )
    )
  )

;;; start-game
;;  Used to start the chain of recursive calls. Keeps track of the move index
;;  for the "best" move.
(defn start-game [state player]
  (let [
        moves (get-valid-moves state player)
        vals (into [] (map #(min-r % -99999999 99999999 0 (switch-player player) player) (vals moves)))
        bestval (apply max vals)
        ]
    (.indexOf vals bestval)
    )
  )
;;; -main
;;  Runs the program.
;;  Reads an input game state from JSON, then calculates the best move
;;  and replies with a JSON object containing the move.
(defn -main
  [& args]
  (doseq [input (repeatedly read-line) :while input]
    (let [parsed (parse-string input) board (get parsed "grid") player (get parsed "player") value (start-game board player)]
      (println (str "{\"move\":" value "}"))
      (binding [*out* *err*]
        (println (str value " is best option."))
        )
      )
    )
  )





