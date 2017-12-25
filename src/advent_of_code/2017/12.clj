(ns advent-of-code.2017.12
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def raw-input (slurp (io/resource "data_2017/12.txt")))

(defn parse-adjacency [line]
  (let [[node & edges] (->> (re-seq #"\d+" line)
                            (map #(Integer/parseInt %)))]
    [node edges]))

(def adjacency-list
  (->> (cs/split-lines raw-input)
       (map parse-adjacency)
       (into (sorted-map))))

(defn visited
  "Given a node return the set of reachable nodes."
  [node]
  (loop [visited #{}
         queue   [node]]
    (if (seq queue)
      (recur (conj visited (peek queue))
             (->> (adjacency-list (peek queue))
                  (remove visited)
                  (into (pop queue))))
      visited)))

;; solve part one
(count (visited 0))

;; solve part two by inefficient naivety
(->> (keys adjacency-list)
     (map visited)
     (distinct)
     (count))
