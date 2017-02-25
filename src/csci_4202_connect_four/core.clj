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
    (apply concat
           (map #(partition 4 1 %)
                (filter
                  #(>= (count %) 4)
                  (apply concat
                         ;; for x from 0-(<state size> - 1)
                         (for [x (range (- 1 (count state)) (count (first state)))]
                           (conj vec (diagonal state x) (diagonal rev x))
                           )
                         )
                  )
                )
           )
    )
  )
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
(defn utility [state player]
  (apply + (map #(points-per-four % player) (concat (get-column-fours state) (get-row-fours state) (get-diagonal-fours state))))
  )

(defn check-diag [state]
  (some true? (map #(when (not= (first %) 0) (apply = %))
                   (get-diagonal-fours state)
                   )
        )
  )
(defn check-column [state]
  (some true?
        (map #(when (not= (first %) 0) (apply = %))
             (get-column-fours state))
        )
  )
(defn check-row [state]
  (some true?
        (map #(when (not= (first %) 0) (apply = %))
             (get-row-fours state))
        )
  )
(defn end-game [state]
  ;; Check each column for an end state
  (or
    (check-row state)
    (check-column state)
    (check-diag state)
    )
  ;; Check each row for end state

  )
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

(defn min-r [state alpha beta depth curr-player player]
  (if (or (nil? state) (= depth MAXDEPTH) (end-game state))
    ;; If max depth, return the utility of this state.
    (utility state player)
    (let [state-map (get-valid-moves state curr-player) size (count state-map)]
      (loop [v 99999999 b-val beta current 0]
        (let [
              state (get state-map current)
              x (min v (max-r state alpha b-val (inc depth) (switch-player curr-player) player))
              ]
          (if (<= x alpha)
            (do
              (binding [*out* *err*]
                ;;(println (str "Pruned at Depth: " depth))
                )
              x)
            (if (= (dec size) current)
              (do
                (binding [*out* *err*]
                  ;;(println (str "All moves explored at depth: " depth " min value: " newbeta))
                  )
                x
                )

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

(defn max-r [state alpha beta depth curr-player player]
  (if (or (nil? state) (= depth MAXDEPTH) (end-game state))
    ;; If max depth, return the utility of this state.
    (utility state player)
    (let [state-map (get-valid-moves state curr-player) size (count state-map)]
      (loop [v -99999999 a-val alpha current 0]
        (let [
              state (get state-map current)
              x (max v (min-r state a-val beta (inc depth) (switch-player curr-player) player))
              ]
          (if (<= beta x)
            (do
              (binding [*out* *err*]
                ;;(println (str "Pruned at Depth: " depth))
                )
              x
              )
            (if (= (dec size) current)
              (do
                (binding [*out* *err*]
                  ;;(println (str "All moves explored at depth: " depth " max value: " newalpha))
                  )
                x
                )

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

;; This is a special variant of the max-r function, which will start the sequence of recursive calls to min-r and max-r.
(defn start-game [state player]
  (let [
        moves (get-valid-moves state player)
        vals (into [] (map #(min-r % -99999999 99999999 0 (switch-player player) player) (vals moves)))
        bestval (apply max vals)
        ]
    (.indexOf vals bestval)
    )
  )
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





