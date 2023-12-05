(ns advent-of-code.2023.3
  (:require [advent-of-code.elves :refer [day->input-lines]]))

(defn indexed-matches [s re]
  (let [matcher (re-matcher re s)]
    (loop [acc []]
      (if (.find matcher)
        (recur (conj acc {:text (.group matcher)
                          :start (.start matcher)
                          :end (dec (.end matcher))}))
        acc))))

(def lines (day->input-lines 2023 3))

(def symbol-locs
  (->> lines
       (map-indexed
        (fn [i l]
          (for [m (indexed-matches l #"[^\.\d]")]
            {:row i :start (:start m)})))
       (apply concat)))

(def numbers
  (->> lines
       (map-indexed
        (fn [i l]
          (for [m (indexed-matches l #"\d+")]
            (assoc m :row i :number (read-string (:text m))))))
       (apply concat)))

(def bounded-numbers
  (for [{:keys [number start end row]} numbers]
    {:number number
     :min-x (dec start) :max-x (inc end)
     :min-y (dec row) :max-y (inc row)}))

(def sym-adj-numbers
  (for [{:keys [number min-x max-x min-y max-y]} bounded-numbers 
        :when (some
               (fn [{:keys [row start]}]
                 (and (<= min-y row max-y)
                      (<= min-x start max-x)))
               symbol-locs)]
    number))

;; part 1 solution
(apply + sym-adj-numbers)

(def gears
  (->> lines
       (map-indexed
        (fn [i l]
          (for [m (indexed-matches l #"\*")]
            {:row i :start (:start m)})))
       (apply concat)))

(def gear-ratios
  (for [{:keys [row start]} gears
        :let [adj-nums
              (filter
               (fn [{:keys [min-x max-x min-y max-y]}]
                 (and (<= min-y row max-y)
                      (<= min-x start max-x)))
               bounded-numbers)]
        :when (= 2 (count adj-nums))]
    (* (:number (first adj-nums))
       (:number (second adj-nums)))))

;; part 2 solution
(apply + gear-ratios)
