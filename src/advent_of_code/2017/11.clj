(ns advent-of-code.2017.11
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def raw-input (slurp (io/resource "data_2017/11.txt")))
(def steps (map keyword (cs/split raw-input #",")))

(defn move [[x y] step]
  (case step
    :n  [x       (+ y 2)]
    :s  [x       (- y 2)]
    :ne [(inc x) (inc y)]
    :nw [(dec x) (inc y)]
    :se [(inc x) (dec y)]
    :sw [(dec x) (dec y)]))

(defn distance [[x y]]
  (let [x (Math/abs x)
        y (Math/abs y)]
    (if (< x y)
      (/ (+ x y) 2) ;; short+wide path = fewer steps
      x)))

;; solve part one
(->> steps
     (reduce move [0 0])
     (distance))

;; solve part two
(->> steps
     (reductions move [0 0])
     (map distance)
     (apply max))
