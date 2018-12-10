(ns advent-of-code.2018.8
  (:require [advent-of-code.elves :refer :all]
            [clojure.string :as cs]
            [clojure.zip :as z]))

(def input
  (mapv read-string
        (-> (day->input-lines 8) (first) (cs/split #" "))))

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

;; solve part one (alternative)
;; build tree w/zipper using non-recursive, stack-based approach
;; adapted from https://www.reddit.com/r/adventofcode/comments/a47ubw/2018_day_8_solutions/ebc99t8/
(defn interpret [input]
  (loop [child-stack [(first input)]
         meta-stack [(second input)]
         i 2
         children [[]]
         meta [[]]
         loc (z/seq-zip ())]
    (if (seq child-stack)
      (cond
        (pos? (peek child-stack))
        (recur
         (-> child-stack
             (update (dec (count child-stack)) dec)
             (conj (nth input i)))
         (conj meta-stack (nth input (inc i)))
         (+ i 2)
         (conj children [])
         (conj meta [])
         (-> loc (z/insert-child ()) z/down))

        (pos? (peek meta-stack))
        (recur
         child-stack
         (update meta-stack (dec (count child-stack)) dec)
         (inc i)
         children
         (update meta (dec (count meta)) conj (nth input i))
         loc)

        :else
        (let [m (peek meta)
              children (pop children)]
          (recur
           (pop child-stack)
           (pop meta-stack)
           i
           (update children (dec (count meta)) conj m)
           (pop meta)
           (do
             (cond-> (z/insert-child loc m)
               (some? (z/up loc)) z/up)))))
      (z/root loc))))
(apply + (flatten (interpret input)))

;; solve part two
(defn node->value [{:keys [child-nodes metadata]}]
  (if (seq child-nodes)
    (transduce
     (comp
      (keep #(nth child-nodes (dec %) nil))
      (map node->value))
     +
     metadata)
    (apply + metadata)))
(node->value parsed)
