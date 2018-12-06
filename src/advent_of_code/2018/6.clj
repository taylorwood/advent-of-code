(ns advent-of-code.2018.6
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def coords
  (->> (io/resource "data_2018/6.txt")
       (slurp)
       (cs/split-lines)
       (map #(map read-string (cs/split % #", ")))))

(defn dist* [[x1 y1] [x2 y2]]
  (+ (Math/abs ^Integer (- x1 x2))
     (Math/abs ^Integer (- y1 y2))))
(def dist (memoize dist*))

(def coord-bounds
  [(apply (juxt min max) (map first coords))
   (apply (juxt min max) (map second coords))])

(def cells
  (let [[[min-x max-x] [min-y max-y]] coord-bounds]
    (for [x (range min-x max-x), y (range min-y max-y)]
      [x y])))

(def outliers ;; coordinates with infinite regions
  (let [[[min-x max-x] [min-y max-y]] coord-bounds]
    (map #(first (sort-by (partial dist %) coords))
         [[min-x min-y] [min-x max-y]
          [max-x min-y] [max-x max-y]])))

(def markers
  (pmap
   (fn [cell]
     (let [[closest next-closest] (sort-by (partial dist cell) coords)
           tie? (= (dist cell closest) (dist cell next-closest))]
       (if tie? '. closest)))
   cells))

;; solve part 1
(time (->> outliers
           (apply dissoc (frequencies markers))
           (apply max-key second)
           (second)))

;; solve part 2
(time (->> cells
           (pmap (fn [cell]
                   (< (apply + (map #(dist cell %) coords)) 10000)))
           (filter true?)
           (count)))
