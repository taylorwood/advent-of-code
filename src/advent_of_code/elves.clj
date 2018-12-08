(ns advent-of-code.elves)

(defn update-vals
  "Returns m with all values applied to f with args."
  [m f & args]
  (persistent! (reduce-kv (fn [m k v] (assoc! m k (apply f v args))) (transient {}) m)))
