(ns advent-of-code.2016.1
  (:require [clojure.string :as cs]))

(def input-raw
  "R4, R4, L1, R3, L5, R2, R5, R1, L4, R3, L5, R2, L3, L4, L3, R1, R5, R1, L3, L1, R3, L1, R2, R2, L2, R5, L3, L4, R4, R4, R2, L4, L1, R5, L1, L4, R4, L1, R1, L2, R5, L2, L3, R2, R1, L194, R2, L4, R49, R1, R3, L5, L4, L1, R4, R2, R1, L5, R3, L5, L4, R4, R4, L2, L3, R78, L5, R4, R191, R4, R3, R1, L2, R1, R3, L1, R3, R4, R2, L2, R1, R4, L5, R2, L2, L4, L2, R1, R2, L3, R5, R2, L3, L3, R3, L1, L1, R5, L4, L4, L2, R5, R1, R4, L3, L5, L4, R5, L4, R5, R4, L3, L2, L5, R4, R3, L3, R1, L5, R5, R1, L3, R2, L5, R5, L3, R1, R4, L5, R4, R2, R3, L4, L5, R3, R4, L5, L5, R4, L4, L4, R1, R5, R3, L1, L4, L3, L4, R1, L5, L1, R2, R2, R4, R4, L5, R4, R1, L1, L1, L3, L5, L2, R4, L3, L5, L4, L1, R3")

(defn get-steps
  "Parses a CSV list of steps into a sequence of tuples."
  [raw]
  (let [xs (->> (cs/split raw #",")
                (map cs/trim))]
    (for [x xs
          :let [dir (keyword (str (first x)))
                len (Integer/parseInt (apply str (rest x)))]]
      [dir len])))

(defn translate-orientation
  "Given a current orientation and a left/right turn, returns the new orientation."
  [orn turn]
  (case [orn turn]
    [:north :R] :east
    [:north :L] :west
    [:east  :R] :south
    [:east  :L] :north
    [:south :R] :west
    [:south :L] :east
    [:west  :R] :north
    [:west  :L] :south))

(defn move-scan
  "Takes the current position, the next step, and returns each intermediate
  position on the way to the destination, and the final position."
  [{:keys [x y orientation] :as state} [dir len]]
  (let [new-orn (translate-orientation orientation dir)
        axis (case new-orn
               (:north :south) :y
               (:east  :west)  :x)
        mv (case new-orn
             (:south :west) -
             (:north :east) +)]
    (for [n (range 1 (inc len))]
      (-> (update state axis mv n)
          (assoc :orientation new-orn)))))

(defn distance
  "Returns the minimum grid distance between two coordinates."
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (let [xd (Math/abs (- x1 x2))
        yd (Math/abs (- y1 y2))]
    (+ xd yd)))

(def starting-pos {:x 0 :y 0 :orientation :north})

;; problem 1 reduce fn only cares about the last/ending pos of each step, not intermediates
(def update-pos (comp last move-scan))
;; solve problem 1
(->> (get-steps input-raw)
     (reduce update-pos starting-pos)
     (distance starting-pos))

;; problem 2 functions
(defn scan
  "Specialized reduce function that accumulates intermediate steps while folding
  over next steps."
  [positions next-step]
  (let [curr (last positions)]
    (concat positions (move-scan curr next-step))))

(defn first-duplicate
  "Returns the first duplicated entry in a sequence, or nil if none exists."
  [xs]
  (let [f (fn [prev c] (if (prev c) (reduced c) (conj prev c)))
        r (reduce f #{} xs)]
    (if (set? r) nil r)))

;; solve problem 2
(->> (get-steps input-raw)
     (reduce scan [starting-pos])
     (map #(dissoc % :orientation))
     (first-duplicate)
     (distance starting-pos))
