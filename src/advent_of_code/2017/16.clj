(ns advent-of-code.2017.16
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def raw-input (slurp (io/resource "data_2017/16.txt")))

(defn spin [coll n]
  (let [part (- (count coll) n)
        tail (subvec coll part)
        head (subvec coll 0 part)]
    (vec (concat tail head))))

(defn swap [coll i j]
  (-> coll
      (assoc i (nth coll j))
      (assoc j (nth coll i))))

(defn positions [pred coll]
  (keep-indexed (fn [i x] (when (pred x) i)) coll))

(defn parse-step [[typ & s]]
  (let [s (apply str s)]
    (case typ
      \s (let [n (Integer/parseInt s)]
           (fn [ps] (spin ps n)))
      \x (let [idxs (->> (re-seq #"\d+" s)
                         (map #(Integer/parseInt %)))]
           (fn [ps] (apply swap ps idxs)))
      \p (let [progset (->> (re-seq #"[a-z]+" s)
                            (map first)
                            (set))]
           (fn [ps] (->> (positions progset ps)
                         (apply swap ps)))))))

(def steps (map parse-step (cs/split raw-input #",")))

(def programs [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p])

;; solve part one
(apply str
  (reduce
    #(%2 %1)
    programs
    steps))

;; After admitting defeat w/brute force I knew I had to
;; be missing a ~key observation~. Given the nature of the
;; permissible moves, there was probably a pattern/cycle
;; repeated many times over 1B iterations...

(defn take-while-distinct [coll]
  (let [seen (volatile! #{})]
    (take-while (fn [v]
                  (if (@seen v)
                    false
                    (vswap! seen conj v)))
                coll)))

(def cycle-len
  (->> (reductions
         #(%2 %1)
         programs
         (cycle steps))
       (take-while-distinct)
       (count)))

;; solve part two
(apply str
  (reduce
    #(%2 %1)
    programs
    ;; problem is now tractable since we know the cycle length
    ;; and can skip (almost all the way) to the end
    (->> (cycle steps)
         (take (* (rem 1000000000 cycle-len)
                  (count steps))))))
