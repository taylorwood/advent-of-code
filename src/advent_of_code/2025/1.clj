(ns advent-of-code.2025.1
  (:require [advent-of-code.elves :refer [day->input-lines]]))

(defn rotation->int [s]
  (let [n (parse-long (subs s 1))]
    (case (subs s 0 1)
      "R" n
      "L" (- n))))

(def rotations
  (map rotation->int (day->input-lines 2025 1)))

;; solve part one
(->> rotations
     (reductions
       (fn [acc elem]
         (mod (+ acc elem) 100))
       50)
     (filter zero?)
     (count))

;; I imagine there's a more efficient/direct mathematical solution
;; instead I lazily generate a sequence of positions for every rotation
(defn clicks [n rot]
  (let [click (if (pos? rot) + -)]
    (for [i (range 1 (inc (abs rot)))]
      (mod (click n i) 100))))

;; solve part two
(->> (reduce
       (fn [acc elem]
         (into acc (clicks (peek acc) elem)))
       [50]
       rotations)
     (filter zero?)
     (count))
