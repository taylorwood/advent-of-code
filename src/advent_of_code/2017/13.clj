(ns advent-of-code.2017.13
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def raw-input (slurp (io/resource "data_2017/13.txt")))

(defn parse-layer [line]
  (mapv #(Integer/parseInt %) (re-seq #"\d+" line)))

(def layers
  (->> (cs/split-lines raw-input)
       (map parse-layer)
       (into (sorted-map))))

;; this solution abuses infinite cycles for the scan positions
;; there should be a pure arithmetic solution to detecting
;; when a scanner is in position zero, but this more fun!

(def layer-scans
  "Map from layer depth to infinite sequence of scan positions."
  (->> layers
       (map (fn [[depth range']]
              (let [scan-idxs (range range')]
                [depth (cycle (concat scan-idxs (rest (reverse (rest scan-idxs)))))])))
       (into {})))

(def layer-seq
  "Infinite sequence of layer scan transitions."
  (iterate (fn [ls] (into {} (map #(update % 1 rest) ls)))
           layer-scans))

(def layer-indices (range (inc (apply max (keys layers)))))

;; solve part one
(->> (map (fn [depth layers']
            (when-let [scan-pos (first (get layers' depth))]
              (when (= scan-pos 0)
                [depth (layers depth)])))
          layer-indices
          layer-seq)
     (keep identity)
     (map (partial apply *))
     (reduce +))

;; using infinite cycles proved way too slow for this
;; so a more ~calculated~ approach is necessary for part two
;; and we just figure out the offsets at which each scanner
;; will be in top position

(def layer-lengths
  (into {}
    (for [[depth range'] layers
          :let [len (- (* 2 range') 2)]]
      [depth len])))

(defn perfect?
  "Returns true if a perfect run is possible for a given delay."
  [delay]
  (every?
    (fn [i]
      (if-let [range-len (layer-lengths i)]
        (not= 0 (mod (+ i delay) range-len))
        true))
    layer-indices))

;; solve part two
(first (filter perfect? (range)))
