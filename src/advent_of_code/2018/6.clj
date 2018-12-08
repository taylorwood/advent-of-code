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
  (let [[[_min-x max-x] [_min-y max-y]] coord-bounds]
    (->> (concat
          (for [x (range max-x)] [x 0])
          (for [y (range max-y)] [0 y])
          (for [x (range max-x)] [x max-y])
          (for [y (range max-y)] [max-x y]))
         (map #(first (sort-by (partial dist %) coords)))
         (set))))

(def markers
  (pmap
   (fn [cell]
     (let [[closest next-closest] (sort-by (partial dist cell) coords)
           tie? (= (dist cell closest) (dist cell next-closest))]
       (if tie? '. closest)))
   cells))

;; solve part 1
(->> outliers
     (apply dissoc (frequencies markers))
     (apply max-key second)
     (second))

;; solve part 2
(->> cells
     (pmap (fn [cell]
             (< (apply + (map #(dist cell %) coords)) 10000)))
     (filter true?)
     (count))

;; viz

(comment
  (require '[quil.core :as q]
           '[quil.middleware :as m])

  (def cells-with-closest
    (pmap
     (fn [cell]
       (let [[closest next-closest] (sort-by (partial dist cell) coords)
             tie? (= (dist cell closest) (dist cell next-closest))]
         [cell (when-not tie? closest)]))
     (let [[[min-x max-x] [min-y max-y]] coord-bounds]
       (for [x (range (+ min-x max-x))
             y (range (+ min-y max-y))]
         [x y]))))

  (def size
    [(apply max (map ffirst cells-with-closest))
     (apply max (map (comp second first) cells-with-closest))])
  (def center (map #(/ % 2) size))
  (def all-colors
    (zipmap (sort-by #(dist % center) coords)
            (rest (take-nth (int (/ 16581375 (count coords)))
                            (for [r (range 255) g (range 255) b (range 255)] [r g b])))))

  (defn draw-state [[coords->color cells]]
    (doseq [[[x y] color] cells]
      (q/set-pixel x y color))
    (q/no-stroke)
    (doseq [[x y] coords
            :let [color (coords->color [x y])
                  rgb [(q/red color)
                       (q/green color)
                       (q/blue color)]]]
      (if (outliers [x y])
        (apply q/fill (map #(min 255 (- % 92)) rgb))
        (apply q/fill (map #(max 0 (+ % 92)) rgb)))
      (q/ellipse x y 4 4)))

  (defn setup []
    (q/background 0)
    (q/frame-rate 5)
    (let [coords->color
          (reduce-kv
           (fn [m coord rgb]
             (assoc m coord (apply q/color
                                   (if (outliers coord)
                                     (map #(min 255 (+ % 64)) rgb)
                                     rgb))))
           {}
           all-colors)]
      [coords->color
       (keep (fn [[cell closest]]
               (when closest
                 [cell (coords->color closest)]))
             cells-with-closest)]))

  (q/defsketch clock
    :title "Chronal Coordinates"
    :size size
    :setup setup
    :draw draw-state
    :middleware [m/fun-mode]
    :features [:keep-on-top]))
