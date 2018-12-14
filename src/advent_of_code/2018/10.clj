(ns advent-of-code.2018.10
  (:require [advent-of-code.elves :refer :all]
            [quil.core :as q]
            [quil.middleware :as m]))

(def input
  (->> (day->input-lines 10)
       (sequence
        (comp
         (map #(re-seq #"\-?\d+" %))
         (map #(map read-string %))
         (map #(zipmap [:x :y :vx :vy] %))
         (map #(assoc % :rgb (if (< 0.5 (rand)) [255 0 0] [0 255 0])))))))

(def input-scale
  {:min-x (:x (apply min-key :x input))
   :min-y (:y (apply min-key :y input))
   :max-x (:x (apply max-key :x input))
   :max-y (:y (apply max-key :y input))})

(defn update-state [{:keys [points]}]
  (let [min-x (:x (apply min-key :x points))
        max-x (:x (apply max-key :x points))
        curr-width (- max-x min-x)
        orig-width (- (:max-x input-scale) (:min-x input-scale))
        aligned-x (count (distinct (map (comp int :x) points)))
        slow-mo? (<= 55 aligned-x 60)
        growth-rate (if slow-mo?
                      (q/map-range aligned-x 55 60 0.001 0.1)
                      (q/map-range curr-width 0 orig-width 0 500))]
    {:slow-mo? slow-mo?
     :points (map
              (fn [{:keys [vx vy] :as point}]
                (-> point
                    (update :x + (* vx growth-rate))
                    (update :y + (* vy growth-rate))))
              points)}))

(defn setup []
  (q/background 0)
  (q/frame-rate 15)
  (q/no-stroke)
  {:points input})

(defn draw-state [{:keys [points slow-mo?]}]
  (q/fill 0 70)
  (q/rect 0 0 (q/width) (q/height))
  (let [min-x (:x (apply min-key :x points))
        min-y (:y (apply min-key :y points))
        max-x (:x (apply max-key :x points))
        max-y (:y (apply max-key :y points))]
    (when slow-mo?
      (dotimes [_ 500]
        (let [rgb (repeatedly 3 #(q/random 0 255))]
          (q/set-pixel (q/random 0 (q/width))
                       (q/random 0 (q/height))
                       (apply q/color rgb)))))
    (doseq [{:keys [x y rgb]} points
            :let [x (q/map-range x min-x max-x 50 (- (q/width) 50))
                  y (q/map-range y min-y max-y 50 (- (q/height) 50))
                  size (if slow-mo? 6 5)]]
      (if slow-mo?
        (apply q/fill (rand-nth [[255 0   0]
                                 [0   255 0]
                                 [0   0   255]
                                 [255 255 0]]))
        (apply q/fill rgb))
      (q/ellipse (int x) (int y) size size))))

;; solve part one
(q/defsketch stars
  :title "The Stars Align"
  :size [1200 250]
  :setup setup
  :draw draw-state
  :update update-state
  :middleware [m/fun-mode]
  :features [:resizable])

;; solved part two by recording fixed precision factors in draw-state
;; then summing them to get the number of seconds elapsed
