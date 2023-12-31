(ns advent-of-code.2019.1
  (:require [advent-of-code.elves :refer :all]))

(def values (map parse-long (day->input-lines 2019 1)))

;; solve part one
(defn mass->fuel [n]
  (- (int (/ n 3)) 2))

(apply + (map mass->fuel values))

;; solve part two
(defn fuel->fuel-mass [n]
  (apply + (take-while pos? (iterate mass->fuel n))))

(apply + (map (comp fuel->fuel-mass mass->fuel) values))
