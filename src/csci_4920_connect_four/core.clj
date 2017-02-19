(ns csci-4920-connect-four.core
  (:gen-class)
  (:require [cheshire.core :refer :all]))

(defn receive-game-string []
  (parse-string (read-line)))

(defn -main
  [& args]
  ;; Bind 'gamestring' to the string parsed from input.
  (let [gamestring (receive-game-string)]
    ;; Bind 'board', 'width', and 'height' to their correseponding values in the map we get from receive-game-string.
    (let [board (get gamestring "grid") width (get gamestring "width") height (get gamestring "height")]
      ;; Print each to validate.
      (println (str "Board: " board))
      (println (str "Width: " width))
      (println (str "Height: " height))
      )
    )
  )







