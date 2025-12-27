(ns advent-of-code.2025.6
  (:require [advent-of-code.elves :refer [day->input-lines]]
            [clojure.string :as cs]))

(def lines
  (vec (day->input-lines 2025 6)))

(def ops
  (map (comp resolve symbol)
       (cs/split (cs/trim (peek lines)) #"\s+")))

;; solve part one
(let [num-lines (pop lines)
      num-grid (for [line num-lines]
                 (map parse-long (cs/split (cs/trim line) #"\s+")))
      ;; transpose to get a group of numbers per column
      num-groups (apply map vector num-grid)]
  (apply +
    (map apply ops num-groups)))

;; solve part two
(let [max-line-len (apply max (map count lines))
      ;; pad lines to max width to make transposition work
      num-lines (for [line (pop lines)]
                  (apply str (take max-line-len (concat line (repeat \space)))))
      transposed (apply map vector num-lines)
      ;; split cols into groups divided by all-space cols
      col-groups (take-nth 2 (partition-by #(every? #{\space} %) transposed))
      ;; parse chars to numbers
      num-groups (for [group col-groups]
                   (map #(parse-long (cs/trim (apply str %))) group))]
  (apply +
    (map apply ops num-groups)))
