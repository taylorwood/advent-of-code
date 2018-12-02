(ns advent-of-code.2018.2
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as cs]))

(def lines
  (->> (io/resource "data_2018/2.txt")
       (slurp)
       (cs/split-lines)))

;; solve part one
(defn score-box-id [id]
  (let [inv-freq (set/map-invert (frequencies id))]
    (cond
      (and (find inv-freq 2) (find inv-freq 3)) :c
      (find inv-freq 2) :a
      (find inv-freq 3) :b)))
(let [{:keys [a b c]} (->> lines (group-by score-box-id))]
  (* (+ (count a) (count c))
     (+ (count b) (count c))))

;; solve part two
(defn diff-indices [a b]
  (keep identity
        (map (fn [x y z] (when-not (= x y) z))
             a b (range))))
(first
 (for [line1 lines
       line2 lines
       :when (not= line1 line2)
       :let [diff (diff-indices line1 line2)]
       :when (= 1 (count diff))]
   (str (subs line1 0 (first diff))
        (subs line1 (inc (first diff))))))
