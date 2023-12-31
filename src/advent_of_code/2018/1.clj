(ns advent-of-code.2018.1
  (:require [advent-of-code.elves :refer :all]))

(def values (map parse-long (day->input-lines 2018 1)))

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
