(ns csci-4202-connect-four.core
  (:gen-class)
  (:require [cheshire.core :refer :all])
  (:import (java.io BufferedReader BufferedWriter)))
(declare min-r)
(declare max-r)

(defn receive-game-string []
  (parse-string (read-line)))


(def MAXDEPTH 3)
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
            (assoc-in state [move (- index 1)] player)

            ))))))

(defn get-valid-moves [state player]
  (into {}
        (map-indexed
          (fn [keyv val] [keyv val])
          (map #(apply-move state % player) (range (count state)))
          )
        )
  )
(defn random-move [state player]
  (loop [states (get-valid-moves state player) num (rand-int (count states))]
    (if-not (nil? (get states num))
      num
      (recur (dissoc states num) (rand-int (count states)))
      )
    )
  )
(defn utility [state]
  (rand-int 100)
  )
(defn end-game [state]
  ;; Check columns for a win state
  (println "Entered")
  ;;(println state)
  (loop [current 5]
    (if-let [col (get state current)]
      (some true? (for [set (partition 4 1 col)] (when (not= (first set) 0) (apply = set))))
      )

    )
  false
  )
(defn switch-player [player]
  (if (= player 1)
    2
    1
    )
  )
(defn min-r [state alpha beta depth player]
  (if (or (nil? state) (end-game state) (= depth MAXDEPTH) )
    ;; If max depth, return the utility of this state.
    (utility state)
    (let [state-map (get-valid-moves state player) size (count state-map)]
      (loop [v 10000 b-val beta current 0]
        (let [
              state (get state-map current)
              x (min v (max-r state alpha beta (inc depth) player))
              newbeta (min b-val x)
              ]
          (if (or (<= b-val alpha) (= size (- current 1)))
            (do
              (println "pruned")
              x)
            (recur x newbeta (inc current))
            )
          )
        )
      )
    )
  )
(defn max-r [state alpha beta depth player]
  (if (or (nil? state) (end-game state) (= depth MAXDEPTH) )
    ;; If max depth, return the utility of this state.
    (utility state)
    (let [state-map (get-valid-moves state player) size (count state-map)]
      (loop [v -10000 a-val alpha current 0]
        (let [
              state (get state-map current)
              x (max v (min-r state alpha beta (inc depth) (switch-player player)))
              newalpha (max a-val x)
              ]
          (if (or (<= beta newalpha) (= size (- current 1)))
            (do
              (println "pruned")
              x)
            (recur x newalpha (inc current))
            )
          )
        )

      )

    )
  )
(defn -main
  [& args]
  (loop []
    ;; Bind 'gamestring' to the string parsed from input.
    (let [gamestring (parse-string (read-line))]
      ;; Bind 'board', 'width', and 'height' to their correseponding values in the map we get from receive-game-string.
      (let [board (get gamestring "grid") width (get gamestring "width") height (get gamestring "height") player (get gamestring "player")]
        ;; Print each to validate.
        (flush)
        (println (generate-string {:move (random-move board player)}))
        (flush)
        ))
    (recur)
    )

  )





