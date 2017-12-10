(ns advent-of-code.2016.6
  (:require [clojure.string :as cs]))

(def raw-input (cs/split-lines (slurp "resources/advent/day-6.txt")))

(defn reduce-slice
  [lns n cmp]
  (->> (map #(nth % n) lns)
       (frequencies)
       (sort-by second cmp)
       (keys)
       (first)))

(->> (for [n (range 8)] ;; part 1
       (reduce-slice raw-input n >))
     (apply str))

(->> (for [n (range 8)] ;; part 2
       (reduce-slice raw-input n <))
     (apply str))
