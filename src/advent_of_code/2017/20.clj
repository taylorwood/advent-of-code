(ns advent-of-code.2017.20
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def raw-input (slurp (io/resource "data_2017/20.txt")))

(defn parse-line [i line]
  (let [triples (->> (re-seq #"-?\d+" line)
                     (map #(Integer/parseInt %))
                     (partition 3))]
    (-> (apply hash-map
               (interleave [:p :v :a]
                           (map vec triples)))
        (assoc :i i)))) ;; tag with particle ID

(def particles
  (->> (cs/split-lines raw-input)
       (map-indexed parse-line)))

(defn move [{:keys [p v a] :as m}]
  (let [new-v (mapv + v a)
        new-p (mapv + p new-v)]
    (assoc m :p new-p :v new-v)))

(defn manhattan-dist [{:keys [p]}] (reduce + (map #(Math/abs %) p)))

(defn tick [particles]
  (map (fn [p] (let [moved (move p)
                     dist (manhattan-dist moved)]
                 (assoc moved :d dist)))
       particles))

;; solve part one via brute force
;; I suspect there's a more elegant solution using ~math~
;; to determine when all particles start never-ending journeys away from zero
(->> (iterate tick particles)
     (pmap (fn [ps] (first (sort-by :d ps))))
     (take 1000)
     (last))

(defn remove-collisions [particles]
  (->> particles
       (group-by :p)
       (filter #(= 1 (count (second %))))
       (mapcat second)))

;; solve part two via brute force too
;; I suspect there's a more elegant solution for this too
;; but I'm not sure how best to determine if there will be another
;; collision without simulation
(->> (iterate (comp remove-collisions tick) particles)
     (map count)
     (take 100)
     (last))
