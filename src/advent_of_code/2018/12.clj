(ns advent-of-code.2018.12
  (:require [advent-of-code.elves :refer :all]
            [clojure.string :as cs]))

(def input (day->input-lines 2018 12))

(def init-pots
  (let [pots-seq (-> (first input)
                     (cs/replace #"^initial state: " "")
                     (seq))]
    (->> pots-seq
         (map-indexed vector)
         (into {}))))

(def patterns
  (->> (drop 2 input)
       (map #(cs/split % #"=>"))
       (map #(map (comp seq cs/trim) %))
       (map (fn [[l r]] [l (first r)]))))

(defn get-pots-pattern [pots offset]
  (->> (range (- offset 2) (+ offset 3) 1)
       (map #(get pots % \.))))

(defn apply-pattern [pattern nc prev-pots new-pots]
  (let [[prev-min prev-max] (apply (juxt min max) (keys prev-pots))]
    (reduce
     (fn [new-pots offset]
       (if (= pattern (get-pots-pattern prev-pots offset))
         (assoc new-pots offset nc)
         new-pots))
     new-pots
     (range (dec prev-min) (+ 2 prev-max)))))

(defn generate [pots]
  (reduce
   (fn [new-pots [pattern output]]
     (apply-pattern pattern output pots new-pots))
   {}
   patterns))

(defn score [generation]
  (->> generation
       (reduce-kv (fn [m k v] (if (= \# v) (assoc m k v) m)) {})
       (keys)
       (apply +)))

(def generations (iterate generate init-pots))

;; solve part 1
(score (nth generations 20))

(defn pots->str [pots]
  (-> (apply str (map second (sort-by first pots)))
      (cs/replace #"^\.+|\.+$" "")))

(->> generations
     (map pots->str)
     (map vector (range))
     (take 150)) ;; pattern starts repeating at 102nd generation

(->> generations
     (drop 102)
     (map score)
     (partition 2)
     (map (partial apply -))
     (take 10)) ;; each subsequent generation score differs by 46

;; solve part 2
(+ (* 46 (- 50000000000 102))
   (score (nth generations 102)))
