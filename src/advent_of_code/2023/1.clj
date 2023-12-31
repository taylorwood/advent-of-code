(ns advent-of-code.2023.1
  (:require [advent-of-code.elves :refer [day->input-lines]])
  (:import [java.lang Character]))

(defn first-digit [s]
  (->> s
       (re-matches #"^.*?(\d).*")
       (second)))

(defn last-digit [s]
  (first-digit (clojure.string/reverse s)))

(defn digit-combo [s]
  (parse-long (str (first-digit s) (last-digit s))))

;; part 1 solution
(apply + (map digit-combo (day->input-lines 2023 1)))

(def word->digit
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn string->digit [s]
  (first
   (for [[k v] word->digit
         :when (clojure.string/starts-with? s k)]
     v)))

(defn parse-digits [s]
  (loop [s s
         acc []]
    (cond
      (empty? s) acc
      
      (Character/isDigit (first s))
      (recur (subs s 1) (conj acc (parse-long (str (first s)))))

      :else (if-let [n (string->digit s)]
              (recur (subs s 1) (conj acc n))
              (recur (subs s 1) acc)))))

;; part 2 solution
(apply +
       (for [l (day->input-lines 2023 1)]
         (let [digits (parse-digits l)]
           (parse-long (str (first digits) (last digits))))))
