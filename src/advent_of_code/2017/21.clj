(ns advent-of-code.2017.21
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def raw-input (slurp (io/resource "data_2017/21.txt")))

(defn parse-pattern [p]
  (let [rows (cs/split p #"/")]
    (mapv (partial mapv {\. 0, \# 1}) rows)))

(defn parse-line [line]
  (let [[input output] (cs/split line #" => ")]
    {:input  (parse-pattern input)
     :output (parse-pattern output)}))

(def pattern-rules (->> (cs/split-lines raw-input)
                        (map parse-line)))

;; matrix permutations
(defn rotate-right [matrix] (apply mapv #(apply vector %&) (reverse matrix)))
(defn flip-vert [matrix] (vec (reverse matrix)))
(defn flip-horiz [matrix] (mapv (comp vec reverse) matrix))
(defn permutations [matrix]
  (distinct ;; HACK a few flips are redundant but I'm short on time!
    (concat
      (take 4 (iterate rotate-right matrix))
      (map flip-horiz (take 4 (iterate rotate-right matrix)))
      (map flip-vert (take 4 (iterate rotate-right matrix))))))

(def input->output
  (memoize ;; HACK make faster
    (fn [matrix]
      (->> pattern-rules
           (filter #(some #{(:input %)} (permutations matrix)))
           (first)))))

(defn sub-matrix [matrix row col size]
  (->> (drop row matrix)
       (take size)
       (mapv #(subvec % col (+ col size)))))

(defn subdivide [matrix]
  (let [size (count matrix)
        mode (if (zero? (rem size 2)) 2 3)]
    (for [row (range 0 size mode)
          col (range 0 size mode)]
      (sub-matrix matrix row col mode))))

(defn append-right "Appends m1 and m2 horizontally."
  ([m1] m1) ;; convenience arity for reduce
  ([m1 m2] (mapv #(vec (apply concat %&)) m1 m2)))
(defn append-bottom "Appends m1 and m2 vertically."
  ([m1] m1)
  ([m1 m2] (vec (concat m1 m2))))

(defn fracture "Grows the matrix by expanding subdivisions in it."
  [matrix]
  (let [submatrices (subdivide matrix)
        newsubs (map (comp :output input->output) submatrices)
        blockcount (count newsubs)]
    (->> newsubs
         (partition (int (Math/sqrt blockcount)))
         (map #(reduce append-right %))
         (reduce append-bottom))))

(def start-pattern [[0 1 0]
                    [0 0 1]
                    [1 1 1]])

;; solve part one
(->> (nth (iterate fracture start-pattern) 5)
     (flatten)
     (reduce +))

;; solve part two
(comment
  "This takes a ~minute to run, but Santa has no time for optimization."
  (->> (nth (iterate fracture start-pattern) 18)
       (flatten)
       (reduce +)))
