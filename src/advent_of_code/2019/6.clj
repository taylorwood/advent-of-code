(ns advent-of-code.2019.6
  (:require [advent-of-code.elves :refer :all]
            [clojure.string :as cs]))

(def values (->> (day->input-lines 2019 6)
                 (map #(vec (reverse (cs/split % #"\)"))))))

(def reverse-adjacency (into {} values))

(defn dist->com [v]
  (letfn [(inner [v n]
            (if-let [nxt (reverse-adjacency v)]
              (recur nxt (inc n))
              n))]
    (inner v 0)))

;; solve part one
;; easy but inefficient; sum each leaf node's distance from COM
(apply + (for [leaf (keys reverse-adjacency)]
           (dist->com leaf)))
