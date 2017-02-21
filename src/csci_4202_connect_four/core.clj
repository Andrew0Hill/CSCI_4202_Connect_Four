(ns csci-4202-connect-four.core
  (:gen-class)
  (:require [cheshire.core :refer :all]))
(declare min-r)
(declare max-r)

(defn receive-game-string []
  (parse-string (read-line)))


(def MAXDEPTH 4)
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
  (into {} (map-indexed (fn [keyv val] [keyv val]) (map #(apply-move state % player) (range (count state)))))
  )

(defn utility [state]
  (rand-int 100)
  )
(defn end-game [state]
  (println "entered")
  false
  )
(defn switch-player [player]
  (if (= player 1)
    2
    1
    )
  )
(defn min-r [state alpha beta depth player]
  (if (or (end-game state) (= depth MAXDEPTH) (nil? state))
    ;; If max depth, return the utility of this state.
    (utility state)
    (loop [v 10000 state-list (get-valid-moves state player) b-val beta]
      (let [
            current (first (remove nil? state-list))
            x (min v (max-r current alpha beta (inc depth) player))
            newbeta (min b-val x)
            ]
        (if (or (<= b-val alpha) (= (count state-list) 1))
          x
          (recur x (drop 1 (remove nil? state-list)) newbeta)
          )
        )
      )
    )
  )
(defn max-r [state alpha beta depth player]
  (if (or (end-game state) (= depth MAXDEPTH) (nil? state))
    ;; If max depth, return the utility of this state.
    (utility state)
    (loop [v -10000 state-list (get-valid-moves state player) a-val alpha]
      (let [
            current (first (remove nil? state-list))
            x (max v (min-r current alpha beta (inc depth) (switch-player player)))
            newalpha (max a-val x)
            ]
        (if (or (<= beta newalpha) (= (count state-list) 1))
          x
          (recur x (drop 1 (remove nil? state-list)) newalpha)
          )
        )
      )
    )
  )
(defn -main
  [& args]
  ;; Bind 'gamestring' to the string parsed from input.
  (let [gamestring (receive-game-string)]
    ;; Bind 'board', 'width', and 'height' to their correseponding values in the map we get from receive-game-string.
    (let [board (get gamestring "grid") width (get gamestring "width") height (get gamestring "height") player (get gamestring "player")]
      ;; Print each to validate.
      (println (max-r board -10000 10000 3 player))

      ))

  )





