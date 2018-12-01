(ns advent-of-code.2017.10
  (:require [clojure.string :as cs]))

(def raw-input "31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33")

(def inputs (map #(Integer/parseInt %) (cs/split raw-input #",")))

(defn twist [buf pos len]
  (let [size (count buf)
        pos (mod pos size) ;; allows for wraparound
        forever (cycle buf)
        twisted (reverse (take len (drop pos forever)))]
    (->> (drop (+ pos len) forever)
         (concat twisted)
         (take size)
         (cycle)
         (drop (- size pos))
         (take size))))

(defn knot-hash [s]
  (reduce
   (fn [{:keys [pos skip hash]} len]
     {:pos  (+ pos len skip)
      :skip (inc skip)
      :hash (twist hash pos len)})
   {:pos 0 :skip 0 :hash (vec (range 256))}
   s))

;; solve part one
(->> inputs
     (knot-hash)
     (:hash)
     (take 2)
     (apply *))

(def char-inputs
  (map (comp int char) raw-input))

(defn knot-hash-2 [s]
  (let [input (concat s [17, 31, 73, 47, 23])]
    (->> (cycle input)
         (take (* (count input) 64))
         (knot-hash)
         (:hash)
         (partition 16)
         (map #(apply bit-xor %))
         (mapcat #(format "%02x" %))
         (apply str))))

;; solve part two
(knot-hash-2 char-inputs)
