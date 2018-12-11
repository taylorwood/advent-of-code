(ns advent-of-code.2018.11
  (:require [clojure.string :as cs]))

(defn cell->power [x y serial]
  (let [rack-id (+ x 10)
        power (* rack-id y)
        power (+ power serial)
        power (* power rack-id)
        hundredth (-> (Math/abs power)
                      (quot 100)
                      (mod 10))]
    (- hundredth 5)))

(defn compute-grid [serial]
  (reduce
   (fn [grid [x y]]
     (update grid x (fnil conj []) (cell->power x y serial)))
   {}
   (for [x (range 1 301)
         y (range 1 301)]
     [x y])))

(def grid (compute-grid 1955))

(defn corner->square [x y size grid]
  (for [x (range x (+ x size))
        y (range y (+ y size))]
    (get-in grid [x (dec y)])))

(defn max-power [size grid]
  (->> (for [x (range 1 (- 301 size))
             y (range 1 (- 301 size))]
         [[x y] (apply + (corner->square x y size grid))])
       (into {})
       (apply max-key second)))

;; solve part 1
(max-power 3 grid)

;; solve part two
(comment
  ;; assuming at some point larger sizes always have smaller sums,
  ;; let this run and pick the answer once the power levels start decreasing
  (doseq [size (range 1 301)]
    (let [[[x y] power] (max-power size grid)]
      (println "power:" power
               "answer:" (cs/join "," [x y size]))))

  ;; or melt your computer to find answer exhaustively
  (pmap max-power (range 1 301)))

;; I considered writing some code to share previously computed
;; size/powers to speed up larger size calculations, but it wasn't
;; necessary.
