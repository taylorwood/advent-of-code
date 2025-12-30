(ns advent-of-code.2025.8
  (:require [advent-of-code.elves :refer [day->input-lines]]
            [clojure.set :as set]
            [clojure.string :as cs]
            [clojure.math.combinatorics :as comb]))

(def coords
  (let [line->coords (fn [s] (mapv parse-long (cs/split s #",")))]
    (mapv line->coords
          (day->input-lines 2025 8))))

(defn distance [coords-a coords-b]
  (Math/sqrt (apply + (map (fn [a b] (Math/pow (- a b) 2)) coords-a coords-b))))

(def connections
  (->> (comb/combinations coords 2)
       (map (juxt identity (partial apply distance)))
       (sort-by second)
       (map first)))

(defn mergeable-circuits [circuits a b]
  (keep
    (fn [c] (when (not-empty (set/intersection c #{a b}))
              c))
    circuits))

(def circuit-states
  "Sequence of circuit states before applying each connection"
  (reductions
    (fn [circuits [a b]]
      (if-let [circuits' (mergeable-circuits circuits a b)]
        (-> (apply disj circuits circuits') ;; merge circuits (remove + add union)
            (conj (apply set/union #{a b} circuits')))
        (conj circuits #{a b})))
    #{}
    connections))

;; solve part one
(let [circuits (nth (rest circuit-states) 1000)]
  (apply * (take 3 (sort-by - (map count circuits)))))

;; solve part two
(let [circuits (drop-while
                 ;; drop til it's one circuit of all coords
                 (fn [[_ state]] (not= state #{(set coords)}))
                 ;; pair distances w/ circuit states
                 (map vector connections (rest circuit-states)))
      [[x1 _ _] [x2 _ _]] (ffirst circuits)]
  (* x1 x2))
