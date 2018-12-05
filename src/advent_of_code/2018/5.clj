(ns advent-of-code.2018.5
  (:require [clojure.java.io :as io]))

(def polymer (slurp (io/resource "data_2018/5.txt")))

(defn reactive? [^Character l ^Character r]
  (and (= (Character/toUpperCase l) (Character/toUpperCase r))
       (or (and (Character/isUpperCase l) (Character/isLowerCase r))
           (and (Character/isUpperCase r) (Character/isLowerCase l)))))

(defn react [polymer]
  (loop [left ()
         right polymer]
    (if-let [[curr & rest] right]
      (recur
        (if (some-> (peek left) (reactive? curr))
          (pop left)
          (conj left curr))
        rest)
      (reverse left))))

;; solve part one
(count (react polymer))

;; solve part two
(def pairs
  (map (juxt identity #(Character/toUpperCase ^Character %))
       [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z]))
(->> pairs
     (pmap
      (fn [[l h]]
        [l (count (react (remove #(or (= l %) (= h %)) polymer)))]))
     (apply min-key second))
