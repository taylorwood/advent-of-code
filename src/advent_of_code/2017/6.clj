(ns advent-of-code.2017.6
  (:require [clojure.string :as cs]))

(def raw-input "5\t1\t10\t0\t1\t7\t13\t14\t3\t12\t8\t10\t7\t12\t0\t6")

(def banks (mapv #(Integer/parseInt %) (cs/split raw-input #"\t")))

(defn max-first-index
  "Finds the index of the first maximal value in coll.
  Ugly but works and I'm already like weeks behind!"
  [coll]
  (when (seq coll)
    (loop [i 0 j 0
           m (first coll)
           xs coll]
      (if (seq xs)
        (let [curr (first xs)
              nm (if m (max curr m) curr)]
          (if (< m nm)
            (recur j (inc j) nm (rest xs))
            (recur i (inc j) nm (rest xs))))
        i))))

(defn distribute [blocks banks offset]
  (loop [blocks blocks
         banks banks
         indices (drop offset (cycle (range (count banks))))]
    (if (pos? blocks)
      (recur (dec blocks)
             (update banks (first indices) inc)
             (rest indices))
      banks)))

(defn redistribute [banks]
  (let [fat-bank-i (max-first-index banks)
        blocks (nth banks fat-bank-i)]
    (distribute blocks
                (assoc banks fat-bank-i 0)
                (inc fat-bank-i))))

(defn first-unique-seq-count
  "Returns the length of the first unique sequence in coll."
  [coll]
  (reduce
    (fn [[i seen] elem]
      (if (seen elem)
        (reduced i)
        [(inc i) (conj seen elem)]))
    [0 #{}]
    coll))

;; solve part one
(first-unique-seq-count (iterate redistribute banks))

;; solve part two, just skip to prev answer offset and repeat
(first-unique-seq-count (drop 5042 (iterate redistribute banks)))
