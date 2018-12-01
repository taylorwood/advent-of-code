(ns advent-of-code.2017.7
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.walk :as walk]))

(def raw-input (slurp (io/resource "data_2017/7.txt")))

(defn parse-line [l]
  (let [[name weight] (rest (re-find #"(\w+)\s\((\d+)\)" l))
        kids (when-let [adj-text (re-find #".+->(.+)" l)]
               (as-> adj-text $
                 (last $)
                 (cs/split $ #",")
                 (map cs/trim $)))]
    {:name     name
     :weight   (Integer/parseInt weight)
     :children kids}))

(def graph (map parse-line (cs/split-lines raw-input)))
(def all-children (set (mapcat :children graph)))

;; solve part one: find the node that has no parents
(def root (first (filter #(not (all-children (:name %))) graph)))

;; part two begins.. convert adjacency list input to proper tree

(defn children
  "Returns any children for a given node and adjacency list."
  [adj-list node]
  (when-let [children (seq (:children node))]
    (for [child-name children]
      (first (filter #(= child-name (:name %)) adj-list)))))

(defn ->tree
  "Converts an adjacency list representation to tree."
  [adj-list node]
  (if-let [kids (children adj-list node)]
    (assoc node :children (map #(->tree adj-list %) kids))
    node))

(def tree
  "Recursive representation of input graph, with child weight sums at each branch."
  (walk/postwalk
   (fn [v]
     (cond
       (and (map? v)
            (seq (:children v)))
       (let [{:keys [weight children]} v
             child-weights (map (some-fn :sum :weight) children)
             child-weight-sum (reduce + child-weights)
             balanced? (every? #(= % (first child-weights))
                               child-weights)]
         (assoc v :sum (+ weight child-weight-sum)
                :balanced? balanced?))
       :else v))
   (->tree graph root)))

;; find the *deepest* off-balance map
(->> tree
     (tree-seq (comp seq :children) :children)
     (filter #(false? (:balanced? %)))
     (last))
;; then I just eyeballed the weights and did the simple math manually
