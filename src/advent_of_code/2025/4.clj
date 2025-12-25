(ns advent-of-code.2025.4
  (:require [advent-of-code.elves :refer [day->input-lines]]))

(def grid
  (mapv (comp vec seq)
        (day->input-lines 2025 4)))

(def adjacent-offsets
  [[-1 -1] [0 -1] [1 -1]
   [-1 0] [1 0]
   [-1 1] [0 1] [1 1]])

(defn paper-locs [grid]
  (for [[row line] (map-indexed vector grid)
        [col cell] (map-indexed vector line)
        :when (= cell \@)]
    [row col]))

(defn adjacent-locs [row col]
  (for [[row-off col-off] adjacent-offsets]
    [(+ row row-off) (+ col col-off)]))

(defn accessible? [grid [row col]]
  (let [neighbors (map #(get-in grid %) (adjacent-locs row col))]
    (> 4 (count (filter #(= \@ %) neighbors)))))

(defn accessible-locs [grid]
  (filter #(accessible? grid %) (paper-locs grid)))

;; solve part one
(count (accessible-locs grid))

;; solve part two
(loop [grid grid
       num-accessed 0]
  (if-let [accessibles (seq (accessible-locs grid))]
    (recur ;; count and clear accessible locs in grid
      (reduce #(assoc-in %1 %2 \.) grid accessibles)
      (+ num-accessed (count accessibles)))
    num-accessed))
