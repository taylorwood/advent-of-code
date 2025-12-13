(ns advent-of-code.2025.3
  (:require [advent-of-code.elves :refer [day->input-lines]]))

(def banks
  (day->input-lines 2025 3))

(defn jolts [bank]
  (let [digits (map (comp parse-long str) bank)
        n1 (apply max (drop-last digits))
        n2 (apply max (rest (drop-while #(not= % n1) digits)))]
    (parse-long (str n1 n2))))

;; solve part one
(apply + (map jolts banks))

(defn max-joltage [bank]
  (loop [batts 12
         digits (map (comp parse-long str) bank)
         acc []]
    (if (zero? batts)
      (parse-long (apply str acc))
      ;; there should be a linear solution but this is fast for the given inputs
      (let [max-num (apply max (drop-last (dec batts) digits))]
        (recur (dec batts)
               (rest (drop-while #(not= % max-num) digits))
               (conj acc max-num))))))

;; solve part two
(apply + (map max-joltage banks))
