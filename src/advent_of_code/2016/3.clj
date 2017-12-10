(ns advent-of-code.2016.3
  (:require [clojure.string :as cs]))

(def raw-input (slurp "day-3.txt"))

(defn ->triangles [txt]
  (->> (cs/split-lines txt)
       (map #(-> (cs/split % #"\s+") (rest)))
       (map (fn [xs] (map #(Integer/parseInt %) xs)))))

(defn valid? [[x y z]]
  (and (< x (+ y z))
       (< y (+ x z))
       (< z (+ x y))))

(->> (->triangles raw-input) ;; part 1
     (filter valid?)
     (count))

(->> (->triangles raw-input) ;; part 2
     (partition 3)
     (mapcat #(apply map vector %))
     (filter valid?)
     (count))
