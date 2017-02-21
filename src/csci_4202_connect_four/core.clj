(ns csci-4202-connect-four.core
  (:gen-class)
  (:require [cheshire.core :refer :all]))
(declare min-r)
(declare max-r)

(defn receive-game-string []
  (parse-string (read-line)))


(def MAXDEPTH 20)
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
            ;; If true recursively call.
            (recur (inc index))
            ;; If false, stop here and associate the given position with the player's number (move).
            (assoc-in state [move (- index 1)] player)))))))

(defn get-valid-moves [state player]
  ;; Start with an empty list and a index 0
  (loop [states () current 0]
    ;; If the current state is larger than the number of the columns in the vector
    (if (= (count state) current)
      ;; Return the list we've made.
      states
      ;; Recursively call the loop, incrementing the count and calling 'apply-move'
      ;; on the curent index.
      (recur (conj states (apply-move state current player)) (inc current)))))

(defn utility [state]

  )
(defn min-r [state alpha beta depth player]
  ;; If we've hit MAXDEPTH, return utility
  (let [states (get-valid-moves state player) current (first states)]
    (if (= depth MAXDEPTH)
      ;; Return the utility of the given state
      (utility current)
        
      )
    )
  )
(defn max-r [state alpha beta depth]
  (if (= depth MAXDEPTH)
    (utility state)

    )
  )

(defn -main
  [& args]
  ;; Bind 'gamestring' to the string parsed from input.
  (let [gamestring (receive-game-string)]
    ;; Bind 'board', 'width', and 'height' to their correseponding values in the map we get from receive-game-string.
    (let [board (get gamestring "grid") width (get gamestring "width") height (get gamestring "height") player (get gamestring "player")]
      ;; Print each to validate.
      (println (str "Board: " board))
      (println (str "Width: " width))
      (println (str "Height: " height)))))





