(ns advent-of-code.2016.4
  (:require [clojure.string :as cs]))

(def raw-input (slurp "day-4.txt"))

(defn ->checksum [txt]
  (->> (frequencies txt)
       (sort-by (juxt #(* -1 (second %)) identity))
       (take 5)
       (map first)
       (apply str)))

(defn eval-line [ln]
  (let [[_ e s ck] (re-matches #"^(.+?)\-(\d+)\[(\w+)\]$" ln)
        e (filter #(not= \- %) e)]
    {:sector-id (Integer/parseInt s)
     :cipher e
     :valid (= ck (->checksum e))}))

(->> (cs/split-lines raw-input) ;; part 1
     (map eval-line)
     (filter :valid)
     (map :sector-id)
     (reduce +))

(def alphabet (mapv char (range 97 (+ 97 26)))) ;; lower ASCII letters

(defn rot-n [n txt]
  (->> (for [c txt
             :let [co (- (byte c) 97)]]
         (nth alphabet (mod (+ co n) 26)))
       (apply str)))

(->> (cs/split-lines raw-input) ;; part 2
     (map eval-line)
     (filter :valid)
     (map #(assoc % :payload (rot-n (:sector-id %) (:cipher %))))
     (filter #(= "northpoleobjectstorage" (:payload %)))
     (map :sector-id))
