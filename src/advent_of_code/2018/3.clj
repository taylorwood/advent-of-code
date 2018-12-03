(ns advent-of-code.2018.3
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn claim->coords [{:keys [x-offset y-offset width height]}]
  (for [x (range x-offset (+ x-offset width))
        y (range y-offset (+ y-offset height))]
    [x y]))

(def claims
  (->> (io/resource "data_2018/3.txt")
       (slurp)
       (cs/split-lines)
       (map #(->> (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" %)
                  (rest)
                  (map read-string)))
       (map #(zipmap [:claim-id :x-offset :y-offset :width :height] %))
       (map #(assoc % :coords (claim->coords %)))))

(defn update-keys [m ks f & args]
  (reduce (fn [m k] (apply update m k f args)) m ks))

;; solve part one
(def cloth
  (reduce
   (fn [acc {:keys [coords claim-id]}] (update-keys acc coords conj claim-id))
   {}
   claims))
(count (filter (fn [[_k v]] (< 1 (count v))) cloth))

;; solve part two
(->> claims
     (filter #(every? (fn [c] (= 1 (count (cloth c))))
                      (:coords %)))
     (first)
     (:claim-id))
