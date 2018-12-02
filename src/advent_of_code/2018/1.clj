(ns advent-of-code.2018.1
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def values
  (->> (io/resource "data_2018/1.txt")
       (slurp)
       (cs/split-lines)
       (map read-string)))

;; solve part one
(apply + values)

;; solve part two
(let [seen (atom #{0})]
  (->> (cycle values)
       (reduce
        (fn [acc v]
          (let [freq (+ acc v)]
            (if (contains? @seen freq)
              (reduced freq)
              (do (swap! seen conj freq)
                  freq)))))))

;; solve part two w/fewer lines
(let [seen (atom #{0})]
  (->> (cycle values)
       (reductions +)
       (drop-while #(apply not= (swap-vals! seen conj %)))
       (first)))

;; solve part two w/o mutability
(->> (cycle values)
     (reductions +)
     (reduce (fn [seen n]
               (if (seen n)
                 (reduced n)
                 (conj seen n)))
             #{0}))

;; NOTE `seen` begins with 0 to handle "+1, -1 first reaches 0 twice" case
;; in which zero is considered as the starting frequency
