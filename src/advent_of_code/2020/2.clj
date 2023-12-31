(ns advent-of-code.2020.2
  (:require [advent-of-code.elves :refer [day->input-lines]]))

(defn valid? [s]
  (let [[_ l h c p] (re-find #"^(\d+)-(\d+)\s([a-z]):\s([a-z]+)$" s)
        l (parse-long l) h (parse-long h)]
    (<= l
        (count (filter #{c} (map str p)))
        h)))

(valid? "9-17 t: mkfttrtvtwdsxxttf")

(count (filter valid? (day->input-lines 2020 2)))

(defn valid-too? [s]
  (let [[_ l h c p] (re-find #"^(\d+)-(\d+)\s([a-z]):\s([a-z]+)$" s)
        l (parse-long l), h (parse-long h)
        a (str (nth p (dec l))), b (str (nth p (dec h)))]
    (and (not= a b)
         (or (= a c) (= b c)))))

(count (filter valid-too? (day->input-lines 2020 2)))
