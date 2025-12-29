(ns advent-of-code.2025.7
  (:require [advent-of-code.elves :refer [day->input-lines]]))

(def lines (day->input-lines 2025 7))

(def beam-ops
  {\S (fn [beam-locs loc]
        (conj beam-locs loc))
   \^ (fn [beam-locs loc]
        (if (beam-locs loc)
          (-> beam-locs
              (disj loc)
              (conj (dec loc) (inc loc)))
          beam-locs))})

;; solve part one
(let [beam-pos-seq
      (reductions ;; calculate beam locations for each line
        (fn [beam-locs line]
          (reduce
            (fn [locs [i ch]]
              (if-let [beam (beam-ops ch)]
                (beam locs i)
                locs))
            beam-locs
            (map-indexed vector line)))
        #{}
        lines)]
  (apply +
    (map ;; count the new beam locations between each line transition
      (fn [[prev curr]] (count (clojure.set/difference prev curr)))
      (partition 2 1 beam-pos-seq))))

(def weight-ops
  {\S (fn [_ curr-locs loc]
        (assoc curr-locs loc 1))
   \^ (fn [prev-weights curr-weights loc]
        (if-let [entry (prev-weights loc)]
          (-> curr-weights
              (update (dec loc) (fnil + 0) entry)
              (update (inc loc) (fnil + 0) entry))
          curr-weights))
   \. (fn [prev-weights curr-weights loc]
        (if-let [entry (prev-weights loc)]
          (update curr-weights loc (fnil + 0) entry)
          curr-weights))})

;; solve part two
(let [beam-weights
      (reduce ;; for each line
        (fn [prev-weights line]
          (reduce ;; for each char in line
            (fn [curr-weights [i ch]]
              ;; calculate number of routes for each beam position
              (if-let [calc-weights (weight-ops ch)]
                (calc-weights prev-weights curr-weights i)
                curr-weights))
            {}
            (map-indexed vector line)))
        {}
        lines)]
  ;; sum the final weights
  (apply + (vals beam-weights)))
