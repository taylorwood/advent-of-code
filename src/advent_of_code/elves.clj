(ns advent-of-code.elves
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn day->input-lines [year day]
  (->> (io/resource (format "data_%s/%s.txt" year day))
       (slurp)
       (cs/split-lines)))

(defn update-vals
  "Returns m with all values applied to f with args."
  [m f & args]
  (persistent! (reduce-kv #(assoc! %1 %2 (apply f %3 args))
                          (transient {})
                          m)))

(defn positions [pred coll]
  (keep-indexed (fn [idx x] (when (pred x) idx)) coll))

(defn digits [n]
  (loop [n n, ds ()]
    (if (zero? n)
      ds
      (recur (quot n 10)
             (cons (mod n 10) ds)))))
