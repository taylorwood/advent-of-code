(ns advent-of-code.2016.10
  (:require [clojure.string :as cs]))

(def raw-input (cs/split-lines (slurp "resources/advent/day-10.txt")))

(defn parse-give [s]
  (let [[_ g lt li ht hi :as x] (re-matches #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)" s)]
    (if x
      [(keyword (str "bot-" g))
       {:lo-dest (keyword (str lt "-" li))
        :hi-dest (keyword (str ht "-" hi))}])))

(def rules (into {} (map parse-give raw-input)))

(defn parse-take [s]
  (let [[_ v t] (re-matches #"value (\d+) goes to bot (\d+)" s)]
    (if (and v t)
      {:bot-id (keyword (str "bot-" (Integer/parseInt t)))
       :value  (Integer/parseInt v)})))

(defn give [acc {b :bot-id v :value}]
  (let [bm (b acc)
        vs (:values bm)]
    (if (empty? vs)
      (assoc acc b (assoc bm :values (conj vs v)))
      (let [ev (first vs)
            [l h] (if (< v ev) [v ev] [ev v])
            ld (:lo-dest bm)
            hd (:hi-dest bm)]
        (println (format "%s gives %s to %s and %s to %s" b l ld h hd))
        (-> (assoc acc b (dissoc bm :values))
            (give {:bot-id (:lo-dest bm) :value l})
            (give {:bot-id (:hi-dest bm) :value h}))))))

(reduce give rules (filter some? (map parse-take raw-input)))
