(ns advent-of-code.elves
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(defn day->input-lines [day]
  (->> (io/resource (format "data_2018/%s.txt" day))
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
