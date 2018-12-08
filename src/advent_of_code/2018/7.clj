(ns advent-of-code.2018.7
  (:require [advent-of-code.elves :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as cs]))

(def lines
  (->> (io/resource "data_2018/7.txt")
       (slurp)
       (cs/split-lines)
       (map #(rest (re-matches #".+\b([A-Z]+)\b.+\b([A-Z]+)\b.+" %)))))

(def child->parents
  (update-vals (group-by second lines)
               #(apply sorted-set (map first %))))

;; solve part one
(def traversal
  (loop [acc []
         rem-child->parents child->parents
         workers (into (set (keys child->parents))
                       (mapcat second child->parents))]
    (if (seq workers)
      (let [curr-node (->> (keys rem-child->parents)
                           (apply disj workers)
                           (sort)
                           (first))]
        (recur
         (or (some->> (workers curr-node) (conj acc))
             acc)
         ;; prune completed relations
         (reduce-kv
          (fn [m child parents]
            (if-let [rem-parents (->> (workers curr-node)
                                      (disj parents)
                                      (not-empty))]
              (assoc m child rem-parents)
              m))
          {}
          rem-child->parents)
         ;; remove completed worker
         (disj workers curr-node)))
      acc)))
(apply str traversal)

;; solve part two
(loop [elapsed 0
       rem-child->parents child->parents
       worker->time (zipmap (sort (into (set (keys child->parents))
                                        (mapcat second child->parents)))
                            (map #(+ 60 (inc %)) (range)))]
  (if (seq worker->time)
    (let [available (apply disj (set (keys worker->time)) (keys rem-child->parents))
          curr-nodes (take 5 (sort available))]
      (recur
       (inc elapsed)
        ;; prune completed relations
       (reduce-kv
        (fn [m child parents]
          (if-let [rem-parents (->> curr-nodes
                                    (filter #(= 1 (worker->time %)))
                                    (apply disj parents)
                                    (not-empty))]
            (assoc m child rem-parents)
            m))
        {}
        rem-child->parents)
        ;; decrement worker elapsed times; prune when complete
       (reduce-kv
        (fn [m worker elapsed]
          (cond
            (not (some #(= worker %) curr-nodes)) (assoc m worker elapsed)
            (<= elapsed 1) m
            :else (assoc m worker (dec elapsed))))
        {}
        worker->time)))
    elapsed))
