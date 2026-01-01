(ns advent-of-code.2025.9
  (:require [advent-of-code.elves :refer [day->input-lines]]
            [clojure.math :as math]
            [clojure.math.combinatorics :as comb]
            [clojure.string :as cs]))

(def coords
  (let [line->coords (fn [s] (mapv parse-long (cs/split s #",")))]
    (mapv line->coords (day->input-lines 2025 9))))

(defn area [corner-a corner-b]
  (* (inc (Math/abs (- (first corner-a) (first corner-b))))
     (inc (Math/abs (- (second corner-a) (second corner-b))))))

;; solve part one
(apply max (map (partial apply area) (comb/combinations coords 2)))

(comment
  ;; my part 2 solution assumes the vertices are defined in clockwise order
  ;; (to determine the interior/exterior side of each edge from its direction)
  ;; which I assessed with this formula
  (defn shoelace-area [vertices]
    (let [points (conj vertices (first vertices))
          sum (->> (partition 2 1 points)
                   (map (fn [[[x1 y1] [x2 y2]]]
                          (- (* x1 y2) (* x2 y1))))
                   (reduce +))]
      (/ sum 2.0)))
  ;; clockwise ex. tl   ;tr   ;br   ;bl
  (shoelace-area [[0 0] [1 0] [1 1] [0 1]])
  ;; counter-clockwise result would be negative
  (shoelace-area coords))

(defn angle [[x1 y1] [x2 y2]]
  (math/to-degrees (math/atan2 (- y2 y1) (- x2 x1))))

(def angle->dir
  {90.0  :down
   0.0   :right
   -90.0 :up
   180.0 :left})

(def directed-edges
  "A map from directions to edges of that direction."
  (let [vertices (conj coords (first coords)) ;; close the loop
        edges (partition 2 1 vertices)]
    (reduce
      (fn [acc [p1 p2]]
        (let [direction (angle->dir (angle p1 p2))]
          (update acc direction conj [p1 p2])))
      {}
      edges)))

(defn cuts? [line1 line2 direction]
  "Returns true if line2 cuts into line1 from given direction."
  (let [[[x1 y1] [x2 y2]] (sort line1)
        [[x1' y1'] [x2' y2']] (sort line2)
        [min-a a max-a, min-b b max-b]
        (if (#{:left :right} direction)
          [y1 y1' y2, x1' x1 x2']
          [x1 x1' x2, y1' y1 y2'])]
    (and (< min-a a max-a)
         (<= min-b b max-b))))

(defn valid? [p1 p2]
  "Returns true if the rect (given by two opposite corners) is not cut into
  by any edge."
  (let [[x1 y1] p1
        [x2 y2] p2
        [left right top bottom] [(min x1 x2) (max x1 x2) (min y1 y2) (max y1 y2)]
        left-edge [[left bottom] [left top]]
        right-edge [[right top] [right bottom]]
        top-edge [[left top] [right top]]
        bottom-edge [[left bottom] [right bottom]]
        cuts (fn [side dir] ;; find edges that cut into given side from direction
               (filter #(cuts? side % dir) (directed-edges dir)))]
    (empty? (concat (cuts left-edge :right)
                    (cuts right-edge :left)
                    (cuts top-edge :down)
                    (cuts bottom-edge :up)))))

(def all-areas
  "All coordinate pairs and their rect areas, largest to smallest."
  (->> (comb/combinations coords 2)
       (map (juxt identity (partial apply area)))
       (sort-by (comp - second))))

;; solve part two
(first
  (for [[[c1 c2] area] all-areas
        :when (valid? c1 c2)]
    area))
