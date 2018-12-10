(ns advent-of-code.2018.9
  (:require [clojure.data.finger-tree :refer :all]))

(defn place-marble [circle marble pos]
  (let [pos (->> (count circle) (range) (cycle) (drop (+ 2 pos)) (first))]
    (if (zero? pos)
      [(conj circle marble) (count circle)]
      [(into (conj (subvec circle 0 pos) marble) (subvec circle pos))
       pos])))

(defn special-marble [circle pos]
  (let [size (count circle)
        remove-pos (->> (range size) (reverse) (cycle) (drop (- size pos)) (take 7) (last))]
    [(into (subvec circle 0 remove-pos) (subvec circle (inc remove-pos)))
     (nth circle remove-pos)
     (first (drop remove-pos (cycle (range size))))]))

(defn scores-slow [marbles players]
  (loop [scores (zipmap (range players) (repeat 0))
         players (cycle (range players))
         circle [0]
         curr-marble-idx 0
         new-marble 1]
    (when (zero? (mod new-marble 100))
      (prn new-marble))
    (if (<= new-marble marbles)
      (if (zero? (mod new-marble 23))
        (let [[new-marbles removed-marble new-pos]
              (special-marble circle curr-marble-idx)]
          (recur
           (update scores (first players) + new-marble removed-marble)
           (rest players)
           new-marbles
           new-pos
           (inc new-marble)))
        (let [[new-marbles new-pos]
              (place-marble circle new-marble curr-marble-idx)]
          (recur scores
                 (rest players)
                 new-marbles
                 new-pos
                 (inc new-marble))))
      scores)))

(comment
  ;; solve part one, slowly
  (time (apply max-key second (scores-slow 70833 486))))

(defn scores [marbles players]
  (loop [scores (zipmap (range players) (repeat 0))
         players (cycle (range players))
         circle (counted-double-list 0)
         new-marble 1]
    (if (<= new-marble marbles)
      (if (zero? (mod new-marble 23))
        (let [[l bonus r] (ft-split-at circle (- (dec (count circle)) 7))]
          (recur (update scores (first players) + new-marble bonus)
                 (rest players)
                 (-> (ft-concat r l) rest (conj (first circle)))
                 (inc new-marble)))
        (recur scores
               (rest players)
               (-> circle rest (conj (first circle)) (conj new-marble))
               (inc new-marble)))
      scores)))

(comment
  ;; solve part two in seconds vs. hours thanks to double-list
  (time (apply max-key second (scores 25 9)))
  (time (apply max-key second (scores 1618 10)))
  (time (apply max-key second (scores 7999 13)))
  (time (apply max-key second (scores 70833 486)))
  (time (apply max-key second (scores (* 100 70833) 486))))
