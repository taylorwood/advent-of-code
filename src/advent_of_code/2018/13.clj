(ns advent-of-code.2018.13
  (:require [advent-of-code.elves :refer :all]))

(def lines (day->input-lines 13))

(def default-cart
  (with-meta {:position [0 0]}
    {:turns (cycle [:left :straight :right])}))

(def char->cell
  {\space :empty
   \|     :vertical
   \/     :diagonal-right
   \\     :diagonal-left
   \-     :horizontal
   \+     :intersection
   \v     (assoc default-cart :direction :south)
   \^     (assoc default-cart :direction :north)
   \>     (assoc default-cart :direction :east)
   \<     (assoc default-cart :direction :west)})

(def init-state
  (loop [rows []
         curr-row []
         carts []
         row-i 0
         col-i 0
         lines lines]
    (if-let [line (first lines)]
      (if (< col-i (count line))
        (let [cell (char->cell (nth line col-i))
              track-piece (if (map? cell)
                            (case (:direction cell)
                              (:north :south) :vertical
                              (:west :east) :horizontal)
                            cell)
              carts (cond-> carts
                      (map? cell) (conj (assoc cell :position [col-i row-i])))]
          (recur rows (conj curr-row track-piece) carts row-i (inc col-i) lines))
        (recur (conj rows curr-row) [] carts (inc row-i) 0 (rest lines)))
      {:rows  rows
       :carts carts})))

(defn next-position [[x y] direction]
  (case direction
    :north [x (dec y)]
    :south [x (inc y)]
    :west [(dec x) y]
    :east [(inc x) y]))

(defn next-direction [cell {:keys [direction] :as cart}]
  (case cell
    (:horizontal :vertical) cart
    :intersection (let [turn (first (:turns (meta cart)))]
                    (-> cart
                        (assoc :direction (case turn
                                            :left (case direction
                                                    :west :south
                                                    :south :east
                                                    :east :north
                                                    :north :west)
                                            :right (case direction
                                                     :west :north
                                                     :south :west
                                                     :east :south
                                                     :north :east)
                                            :straight direction))
                        (vary-meta update :turns rest)))
    :diagonal-left (assoc cart :direction (case direction
                                            :east :south
                                            :south :east
                                            :west :north
                                            :north :west))
    :diagonal-right (assoc cart :direction (case direction
                                             :north :east
                                             :south :west
                                             :east :north
                                             :west :south))))

(defn tick [{:keys [rows carts] :as state}]
  (let [sorted-carts (vec (sort-by (comp vec reverse :position) carts))]
    (reduce
     (fn [{:keys [carts] :as state} cart-index]
       (let [{:keys [direction position] :as cart} (nth carts cart-index)]
         (let [new-pos (next-position position direction)
               cart (-> (get-in rows (reverse new-pos))
                        (next-direction cart))]
           (if-let [collide-index (->> carts
                                       (positions #(and (not (:collided? %))
                                                        (= (:position %) new-pos)))
                                       (first))]
             (-> state
                 (assoc-in [:carts cart-index] (assoc cart :position new-pos :collided? true))
                 (assoc-in [:carts collide-index] (assoc cart :position new-pos :collided? true))
                 (update :collisions conj new-pos))
             (assoc-in state [:carts cart-index] (assoc cart :position new-pos))))))
     (assoc state :carts sorted-carts)
     (positions (comp not :collided?) sorted-carts))))

;; solve part one
(comment
  (->> (iterate tick init-state)
       (drop-while (comp not :collisions))
       (first)
       (:collisions)))

;; solve part two
(comment
  (->> (iterate tick init-state)
       (drop-while #(not= (count (filter :collided? (:carts %))) (dec (count (:carts %)))))
       (first)
       (:carts)))
