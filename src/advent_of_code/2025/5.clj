(ns advent-of-code.2025.5
  (:require [advent-of-code.elves :refer [day->input-lines]]
            [clojure.string :as cs]))

(def ranges-ids
  (let [[ranges _ ids] (partition-by empty? (day->input-lines 2025 5))
        parse-range (fn [s] (mapv parse-long (cs/split s #"-")))
        ranges (mapv parse-range ranges)
        ids (mapv parse-long ids)]
    [ranges ids]))
(def ranges (first ranges-ids))
(def ids (second ranges-ids))

;; solve part one
(count
  (filter
    #(some (fn [[lo hi]] (<= lo % hi)) ranges)
    ids))

;; solve part two
(loop [ranges (sort ranges)
       counter 0
       base 0]
  (if (seq ranges)
    (let [[lo hi] (first ranges)
          hi (inc hi) ;; for inclusive range math
          base (max lo base)]
      (recur
        (rest ranges)
        (+ counter (max 0 (- hi base)))
        (max base hi)))
    counter))
