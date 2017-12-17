(ns advent-of-code.2017.9
  (:require [clojure.java.io :as io]))

(def raw-input (slurp (io/resource "data_2017/9.txt")))

(defn eat-garbage [string]
  (loop [s string]
    (case (first s)
      nil nil
      \! (recur (drop 2 s))
      \> (rest s)
      (recur (rest s)))))

(defn group-score-sum
  "Consumes groupings from string. Returns group score sum."
  [string]
  (loop [s string
         opens 0
         sum 0]
    (case (first s)
      nil sum
      \{ (recur (rest s) (inc opens) (+ sum (inc opens)))
      \} (recur (rest s) (dec opens) sum)
      \< (recur (eat-garbage s) opens sum)
      (recur (rest s) opens sum))))

(group-score-sum raw-input) ;; solve part one

(defn count-garbage
  "Returns count of characters from garbage sections of string."
  [string]
  (loop [s string
         garbage? false
         sum 0]
    (case (first s)
      nil sum
      \< (recur (rest s) true (if garbage? (inc sum) sum))
      \! (recur (drop 2 s) garbage? sum)
      \> (recur (rest s) false sum)
      (recur (rest s) garbage? (if garbage? (inc sum) sum)))))

(count-garbage raw-input) ;; solve part two
