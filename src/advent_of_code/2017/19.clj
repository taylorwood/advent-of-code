(ns advent-of-code.2017.19
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def raw-input (slurp (io/resource "data_2017/19.txt")))

(def grid "2D vector of chars in circuit diagram."
  (->> (cs/split-lines raw-input)
       (mapv (comp vec seq))))

(defn nth* [coll i & is]
  (reduce
    #(nth %1 %2 nil)
    (nth coll i nil)
    is))

(defn next-pos [[row col] bearing]
  (case bearing
    :n [(dec row) col]
    :s [(inc row) col]
    :e [row (inc col)]
    :w [row (dec col)]))

(defn is-letter? [c] (and (char? c) (Character/isLetter ^Character c)))

(defn intersection-bearings [grid [row col]]
  (let [valid-chars {:n \| :s \| :e \- :w \-}]
    (for [[bearing validc] valid-chars
          :let [nextp (next-pos [row col] bearing)
                nextc (apply nth* grid nextp)]
          :when (or (= nextc validc) (is-letter? nextc))]
      [bearing nextp])))

(def complement-bearing {:n :s, :s :n, :e :w, :w :e})

(def steps (atom 0)) ;; this is an incredibly lazy hack for pt.2 but will anyone ever read this code?!

(defn visited-letters [diagram]
  (loop [curr-pos [0 (-> (first diagram) (.indexOf \|))]
         bearing :s
         visited []]
    (swap! steps inc)
    (if-let [curr-char (apply nth* diagram curr-pos)]
      (cond
        (= \+ curr-char) ;; turn at intersections
        (let [[next-bearing next-pos] (->> (intersection-bearings diagram curr-pos)
                                           (remove #(= (first %) (complement-bearing bearing)))
                                           (first))] ;; first valid, forward path leaving intersection
          (recur next-pos next-bearing visited))

        (is-letter? curr-char) ;; collect visited letters and proceed
        (let [next-pos (next-pos curr-pos bearing)]
          (recur next-pos bearing (conj visited curr-char)))

        (= \space curr-char) ;; ran out of circuit
        visited

        :else ;; follow circuit
        (let [next-pos (next-pos curr-pos bearing)]
          (recur next-pos bearing visited)))
      visited)))

;; solve part one
(apply str (visited-letters grid))
;; solve part two
(dec @steps)
