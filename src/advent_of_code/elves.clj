(ns advent-of-code.elves
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn day->input [year day]
  (->> (io/resource (format "data_%s/%s.txt" year day))
       (slurp)))

(defn day->input-lines [year day]
  (cs/split-lines (day->input year day)))

(defn positions [pred coll]
  (keep-indexed (fn [idx x] (when (pred x) idx)) coll))

(defn digits [n]
  (loop [n n, ds ()]
    (if (zero? n)
      ds
      (recur (quot n 10)
             (cons (mod n 10) ds)))))
