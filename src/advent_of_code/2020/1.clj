(ns advent-of-code.2020.1
  (:require [advent-of-code.elves :refer [day->input-lines]]))

(let [nums (map parse-long (day->input-lines 2020 1))]
  (first
   (for [x nums
         y nums
         :when (= 2020 (+ x y))]
     (* x y))))

(let [nums (map parse-long (day->input-lines 2020 1))]
  (first
   (for [x nums
         y nums
         z nums
         :when (= 2020 (+ x y z))]
     (* x y z))))
