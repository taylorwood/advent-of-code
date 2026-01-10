(ns advent-of-code.2025.10
  (:require [advent-of-code.elves :refer [day->input-lines]]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [clojure.string :as cs]))

(def parsed
  (for [line (day->input-lines 2025 10)]
    (let [[light-targets & parts] (cs/split line #" ")]
      [;; parse target light state
       (let [len (count light-targets)]
         (mapv (fn [c] (if (= c \#) 1 0)) (subs light-targets 1 (dec len))))
       ;; parse button specs
       (map (fn [button]
              (let [len (count button)]
                (mapv parse-long (cs/split (subs button 1 (dec len)) #","))))
            (butlast parts))
       ;; parse target joltage state
       (let [joltage-targets (last parts)
             len (count joltage-targets)]
         (mapv parse-long (cs/split (subs joltage-targets 1 (dec len)) #",")))])))

(defn button-combo [s1 s2]
  (set/union (set/difference s1 s2)
             (set/difference s2 s1)))

(defn solutions [target-state button-sets]
  (let [buttons (map set button-sets)
        target-set (set (keep-indexed (fn [i state] (when (= 1 state) i))
                                      target-state))]
    ;; for every possible subset of button presses, compare their ending state to target
    (for [button-combos (rest (comb/subsets buttons))
          :when (= target-set (reduce button-combo button-combos))]
      button-combos)))

;; solve part one
(apply + (for [[target-state button-sets _] parsed]
           (count (first (solutions target-state button-sets)))))
