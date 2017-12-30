(ns advent-of-code.2017.14
  (:require [advent-of-code.2017.10 :refer [knot-hash-2]]
            [clojure.set :refer :all]))

(def input "ffayrhll")

(defn hex->bits [h]
  (->> (Integer/parseInt (str h) 16)
       (Integer/toBinaryString)
       (str "0000")
       (take-last 4)
       (map #(case % \0 0 \1 1))))

(def row-inputs
  (map #(str input "-" %) (range 128)))

(defn hash-row [r]
  (->> (map int r)
       (knot-hash-2)
       (mapcat hex->bits)))

(def row-hashes
  (vec (pmap (comp vec hash-row) row-inputs)))

;; solve part one
(->> row-hashes
     (flatten)
     (reduce +))

(defn adjacent-offsets [row col]
  [[(dec row) col], [row (dec col)], [row (inc col)], [(inc row) col]])

(def occupied-offsets
  (sequence
    (comp
      (map-indexed (fn [i row]
                     (let [lit-cols (keep-indexed #(when (= 1 %2) %1) row)]
                       (map vector (repeat i) lit-cols))))
      (mapcat identity))
    row-hashes))

(def regions
  (reduce
    (fn [acc [row col]]
      (let [adj-offs (set (adjacent-offsets row col))
            adj-regions (filter #(some adj-offs %) acc)
            new-region (conj (apply union adj-regions) [row col])]
        (-> (apply disj acc adj-regions) ;; remove old adjacent regions
            (conj new-region))))         ;; add new region
    #{}
    occupied-offsets))

;; solve part two
(count regions)
