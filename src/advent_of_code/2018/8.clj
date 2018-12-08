(ns advent-of-code.2018.8
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def input
  (->> (cs/split (slurp (io/resource "data_2018/8.txt")) #" ")
       (map read-string)))

(defn parse-tree [input]
  (letfn [(parse-node [[num-kids num-meta & more]]
            (let [[children more'] (loop [children' []
                                          n num-kids
                                          rem more]
                                     (if (pos? n)
                                       (let [[node rem'] (parse-node rem)]
                                         (recur (conj children' node) (dec n) rem'))
                                       [children' rem]))]
              [{:child-nodes children
                :metadata (take num-meta more')}
               (drop num-meta more')]))]
    (first (parse-node input))))

(def parsed (parse-tree input))

;; solve part one
(->> parsed
     (tree-seq map? :child-nodes)
     (mapcat :metadata)
     (apply +))

(defn node->value [{:keys [child-nodes metadata]}]
  (if (seq child-nodes)
    (transduce
     (comp
      (keep #(nth child-nodes (dec %) nil))
      (map node->value))
     +
     metadata)
    (apply + metadata)))

;; solve part two
(node->value parsed)
