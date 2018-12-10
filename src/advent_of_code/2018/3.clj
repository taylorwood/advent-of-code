(ns advent-of-code.2018.3
  (:require [advent-of-code.elves :refer :all]))

(defn claim->coords [{:keys [x-offset y-offset width height]}]
  (for [x (range x-offset (+ x-offset width))
        y (range y-offset (+ y-offset height))]
    [x y]))

(def claims
  (->> (day->input-lines 3)
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

(comment
  "Render animated elven tapestry."
  (require '[quil.core :as q]
           '[quil.middleware :as m])

  (defn draw-state [{:keys [claims]}]
    (if-let [claim (first claims)]
      (let [{:keys [x-offset y-offset width height color]} claim]
        (apply q/fill (conj color 192))
        (q/rect x-offset y-offset width height))
      (q/no-loop)))

  (def colors
    (let [step (range 0 256 25)]
      (for [r step g step b step] [r g b])))

  (defn setup []
    (q/frame-rate 60)
    (q/background 255)
    {:claims (map #(assoc %1 :color %2) claims colors)})

  (q/defsketch matrix
    :title "No Matter How You Slice It"
    :size [(apply max (map first (keys cloth)))
           (apply max (map second (keys cloth)))]
    :setup setup
    :update #(update % :claims rest)
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
