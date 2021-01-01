(ns advent-of-code.2020.3
  (:require [advent-of-code.elves :refer [day->input-lines]]))

(def input-lines (day->input-lines 2020 3))
(def width (count (first input-lines)))

(defn rot-offsets [step]
  (iterate #(mod (+ step %) width)
           0))

(defn solve [step lines]
  (count
    (filter #(= \# %)
            (map nth
                 lines
                 (rot-offsets step)))))

(*
  (solve 1 input-lines)
  (solve 3 input-lines) ;; part 1
  (solve 5 input-lines)
  (solve 7 input-lines)
  (solve 1 (take-nth 2 input-lines))) ;; part 2
