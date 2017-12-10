(ns advent-of-code.2016.8
  (:require [clojure.string :as cs]))

(def raw-input (cs/split-lines (slurp "resources/advent/day-8.txt")))

(defn parse-rect [s]
  (if-let [ms (re-matches #"rect (\d+)x(\d+)" s)]
    (let [[_ x y] ms]
      {:type :rect :width (Integer/parseInt x) :height (Integer/parseInt y)})))

(defn parse-rotate [s]
  (if-let [ms (re-matches #"rotate (row|column) (.=(\d+)) by (\d+)" s)]
    (let [[_ a _ i d] ms]
      {:type     :rotate
       :axis     (case a "column" :x "row" :y)
       :position (Integer/parseInt i)
       :distance (Integer/parseInt d)})))

(def cmds (map #(or (parse-rect %) (parse-rotate %)) raw-input))

(defn rect->cells [{w :width h :height}]
  (flatten (for [x (range w)]
             (for [y (range h)]
               {:x x :y y}))))

(defn draw-rect [scr rect]
  (let [cs (rect->cells rect)
        on (fn [acc {x :x y :y}] ;; turns a single pixel on
             (let [row (nth acc y)]
               (assoc acc y (assoc row x 1))))]
    (reduce on scr cs)))

(defn rotate-axis [scr {p :position d :distance}]
  (let [r (nth scr p)
        n' (- (count r) d)
        c (into [] (concat (drop n' r) (take n' r)))]
    (assoc scr p c)))

(defn transpose [m] (apply mapv vector m))

(defn rotate [scr cmd]
  (case (:axis cmd)
    :x (-> (transpose scr) ;; handle cross-row shifts by pre- & post-transposition
           (rotate-axis cmd)
           (transpose))
    :y (rotate-axis scr cmd)))

(defn apply-cmd [scr cmd]
  (case (:type cmd)
    :rotate (rotate scr cmd)
    :rect   (draw-rect scr cmd)))

(def screen (vec (repeat 6 (vec (repeat 50 0))))) ;; 50x6 grid

(def lit (reduce apply-cmd screen cmds))

(count (filter #(= 1 %) (flatten lit))) ;; solve part 1

(defn print-scr [scr]
  (doseq [r scr]
    (->> (map #(if (= 0 %) " " "#") r)
         (apply str)
         (println))))

(print-scr lit) ;; solve part 2
