(ns advent-of-code.2019.3
  (:require [advent-of-code.elves :refer :all]))

(def values (day->input-lines 2019 3))

(defn parse-moves [s]
  (->> (clojure.string/split s #",")
       (map (fn [instruct]
              {:dir (first instruct) :len (Integer/parseInt (subs instruct 1))}))))

(def line-1 (parse-moves (first values)))
(def line-2 (parse-moves (second values)))

(defn line-cells [pos {:keys [dir len]}]
  (let [move (case dir
               \U #(update % :y inc)
               \D #(update % :y dec)
               \L #(update % :x dec)
               \R #(update % :x inc))]
    (take len (rest (iterate move pos)))))

(defn instructions->cells [is]
  (second
   (reduce
     (fn [[pos cells] instruct]
       (let [path-cells (line-cells pos instruct)]
         [(last path-cells) (into cells path-cells)]))
     [{:x 0 :y 0} []]
     is)))

(defn distance [{x :x y :y} {x' :x y' :y}]
  (+ (Math/abs (int (- x x')))
     (Math/abs (int (- y y')))))

;; solve part one
(->> (clojure.set/intersection
      (set (instructions->cells line-1))
      (set (instructions->cells line-2)))
     (map (juxt identity (partial distance {:x 0 :y 0})))
     (apply min-key second))

;; solve part two
(let [cells->steps (fn [instructs]
                     (into {} (reverse (map vector ;; reverse so earlier steps take precedence in last-write-wins map construction
                                            (instructions->cells instructs)
                                            (rest (range))))))
      cell-steps-1 (cells->steps line-1)
      cell-steps-2 (cells->steps line-2)
      intersects (clojure.set/intersection
                  (set (map first cell-steps-1))
                  (set (map first cell-steps-2)))
      intersect-step-counts (map + (map cell-steps-1 intersects) (map cell-steps-2 intersects))]
  (apply min intersect-step-counts))
