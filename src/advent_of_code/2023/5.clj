(ns advent-of-code.2023.5
  (:require [advent-of-code.elves :refer [day->input-lines]]
            [clojure.string :as cs]))

(def input (day->input-lines 2023 5))

(def seeds
  [5844012 110899473 1132285750 58870036 986162929 109080640 3089574276 100113624 2693179996 275745330 2090752257 201704169 502075018 396653347 1540050181 277513792 1921754120 26668991 3836386950 66795009])

(def parse-sections
  (partition 2
             (partition-by cs/blank? (rest input))))

(defn parse-map-line [s]
  (let [[dest src len] (map parse-long (cs/split s #" "))]
    [dest src len]))

(def maps
  (for [[_ sect] parse-sections
        :let [[title & nums] sect]]
    {:name title
     :ranges (map parse-map-line nums)}))

(defn destination [seed ranges]
  (if-let [x (first (for [[dest src len] ranges
                          :when (<= src seed (+ src len))]
                      (+ dest (- seed src))))]
    x
    seed))

;; part 1 solution
(apply min
  (for [seed seeds]
    (reduce
      (fn [seed {:keys [ranges]}]
        (destination seed ranges))
      seed
      maps)))

;; work backwards from locations?
;; model as range shifts; would normalizing the ranges help?

(def seed-ranges
  (map (fn [[m l]] [m (+ m l)])
       (sort-by first (partition 2 seeds))))

(def min-seed (apply min (map first seed-ranges)))
(def max-seed (apply max (map #(apply + %) seed-ranges)))
(- max-seed min-seed)

(defn parse-map-line [s]
  (let [[dest src len] (map parse-long (cs/split s #" "))]
    [[src (+ src len)]
     [dest (+ dest len)]]))

(parse-map-line "0 50 50")

(def maps
  (for [[_ sect] parse-sections
        :let [[title & nums] sect]]
    {:name title
     :ranges (sort-by second (map parse-map-line nums))}))

(defn overlap? [[a b] [x y]]
  (and (<= a y)
       (>= b x)
       [(max a x) (min b y)]))

(defn test-overlap [m]
  (for [[a b] (partition 2 1 (sort (map first (:ranges m))))]
    (overlap? a b)))

(sort-by second (:ranges (last maps)))
