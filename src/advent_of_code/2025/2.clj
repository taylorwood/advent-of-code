(ns advent-of-code.2025.2
  (:require [advent-of-code.elves :refer [day->input]]
            [clojure.string :as cs]))

(def ranges
  (for [l (cs/split (day->input 2025 2) #",")]
    (mapv parse-long (cs/split l #"-"))))

(def all-nums
  (for [[lo hi] ranges
        n (range lo (inc hi))]
    n))

(defn invalid? [s]
  (let [len (count s)
        chs (seq s)]
    (apply = (split-at (/ len 2) chs))))

;; solve part one
(apply +
  (filter #(invalid? (str %)) all-nums))

(defn count-prefix-repeat [coll]
  "If coll is made of a repeated prefix, returns the number of
  repetitions for the shortest such prefix."
  (let [len (count coll)]
    (first
      (for [i (range 1 (inc (/ len 2)))
            :when (zero? (mod len i))
            ;; (apply = (partition i coll)) also works, but slower
            :let [prefix (take i coll)]
            :when (= coll (take len (cycle prefix)))]
        (/ len i)))))

;; solve part two
(apply +
  (for [n all-nums
        :let [reps (count-prefix-repeat (seq (str n)))]
        :when (and reps (<= 2 reps))]
    n))
