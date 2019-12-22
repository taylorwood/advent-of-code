(ns advent-of-code.2019.4
  (:require [advent-of-code.elves :refer :all]))

(def password-range (range 231832 (inc 767346)))

(defn is-password? [n]
  (let [ds (digits n)]
    (and (apply <= ds)
         (some (partial apply =) (partition 2 1 ds)))))

;; solve part one
(count (filter is-password? password-range))

;; solve part two
(defn is-password-too? [n]
  (let [ds (digits n)]
    (and (apply <= ds)
         (->> ds
              (partition-by identity)
              (map count)
              (some #(= 2 %))))))

(count (filter is-password-too? password-range))
