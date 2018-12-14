(ns advent-of-code.2018.14
  (:require [advent-of-code.elves :refer :all]))

(def recipes-part-1
  (loop [recipes [3 7]
         left 0
         right 1
         i 880751]
    (if (pos? i)
      (let [l-move (inc (nth recipes left))
            r-move (inc (nth recipes right))
            sum (+ (nth recipes left) (nth recipes right))
            new-recipes (if (<= 10 sum)
                          (into recipes [(quot sum 10) (mod sum 10)])
                          (conj recipes sum))]
        (recur new-recipes
               (mod (+ left l-move) (count new-recipes))
               (mod (+ right r-move) (count new-recipes))
               (dec i)))
      recipes)))

;; solve part one
(apply str (take 10 (drop 880751 recipes-part-1)))

(def recipes-part-2 ;; takes ~7s to find input
  (loop [recipes [3 7]
         left 0
         right 1]
    (let [l-move (inc (nth recipes left))
          r-move (inc (nth recipes right))
          sum (+ (nth recipes left) (nth recipes right))
          new-recipes (if (<= 10 sum)
                        (into recipes [(quot sum 10) (mod sum 10)])
                        (conj recipes sum))
          recipe-count (count new-recipes)]
      (cond
        ;; when the recipes end with our input
        (and (<= 6 recipe-count)
             (= [8 8 0 7 5 1] (subvec new-recipes (- recipe-count 6))))
        (subvec new-recipes 0 (- recipe-count 6))
        ;; there may be a trailing/extra digit we don't care about
        (and (<= 7 recipe-count)
             (= [8 8 0 7 5 1]
                (subvec new-recipes (- recipe-count 7)
                        (+ 6 (- recipe-count 7)))))
        (subvec new-recipes 0 (- recipe-count 7))
        ;; keep cooking
        :else (recur new-recipes
                     (mod (+ left l-move) recipe-count)
                     (mod (+ right r-move) recipe-count))))))

;; solve part two
(count recipes-part-2)
