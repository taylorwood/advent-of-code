(ns advent-of-code.2017.22
  (:require [advent-of-code.2017.19 :refer [next-pos reverse-bearing]]
            [clojure.java.io :as io]
            [clojure.set :refer [map-invert]]
            [clojure.string :as cs]))

(def raw-input (slurp (io/resource "data_2017/22.txt")))

(def start-grid
  (->> (cs/split-lines raw-input)
       (map #(map {\. 0 \# 1} %))))

(def center-pos [(int (/ (count start-grid) 2))
                 (int (/ (count (first start-grid)) 2))])

(def infected-cells
  (->> (for [row start-grid]
         (keep-indexed #(when (= 1 %2) %1) row))
       (map-indexed (fn [i row] (map vector (repeat i) row)))
       (mapcat identity)
       (into #{})))

(def turn-right {:n :e, :e :s, :s :w, :w :n})
(def turn-left (map-invert turn-right))

(defn burst [{:keys [infected curr-pos bearing] :as state}]
  (let [curr-infected? (contains? infected curr-pos)
        new-bearing (if curr-infected? (turn-right bearing) (turn-left bearing))
        new-pos (next-pos curr-pos new-bearing)
        clean-or-infect (if curr-infected? disj conj)]
    (-> state
        (assoc :curr-pos new-pos :bearing new-bearing)
        (update :infected clean-or-infect curr-pos)
        (update :infections #(if-not curr-infected?
                               (inc (or % 0))
                               %)))))

;; solve part one
(-> (iterate burst {:curr-pos center-pos :bearing :n :infected infected-cells})
    (nth 10000)
    (:infections))

(def cell-transition {:clean :weakened, :weakened :infected, :infected :flagged, :flagged :clean})

;; this uses a set of visited positions and tracks cell state with metadata
(defn burst-2 [{:keys [visited curr-pos bearing] :as state}]
  (let [curr-pos-state (or (some-> (visited curr-pos)
                                   (meta)
                                   (:state))
                           :clean)
        new-pos-state (cell-transition curr-pos-state)
        new-bearing (case curr-pos-state
                      :clean (turn-left bearing)
                      :weakened bearing
                      :infected (turn-right bearing)
                      :flagged (reverse-bearing bearing))]
    (-> state
        (update :visited #(-> %
                              (disj curr-pos)
                              (conj (with-meta curr-pos {:state new-pos-state}))))
        (assoc :curr-pos (next-pos curr-pos new-bearing)
               :bearing new-bearing)
        (update :infections #(if (= :infected new-pos-state)
                               (inc (or % 0))
                               %)))))

;; solve part two
(-> (iterate burst-2 {:curr-pos center-pos
                      :bearing  :n
                      :visited  (->> infected-cells
                                     (map #(with-meta % {:state :infected}))
                                     (into #{}))})
    (nth 10000000)
    (:infections))
