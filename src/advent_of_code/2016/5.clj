(ns advent-of-code.2016.5
  (:require [clojure.string :as cs])
  (:import (java.security MessageDigest)))

(defn md5 [txt]
  (->> (.digest (doto (MessageDigest/getInstance "MD5")
                  (.update (.getBytes txt))))
       (map #(format "%02x" %))
       (apply str)))

(->> (range) ;; solve part 1
     (pmap #(md5 (str "abbhdwsy" %)))
     (filter #(cs/starts-with? % "00000"))
     (take 8)
     (map #(nth % 5)))

(defn valid-pos? [pos ans]
  (if (and (<= 48 (int pos) 55) ;; is `pos` in ASCII 0 ... 7?
           (not (ans pos)))     ;; is `pos` not already figured out?
    pos))

(def unsolved-mystery (zipmap (map char (range 48 56)) (repeat \_)))
(defn print-answer [m] ;; extra credit progressive answer printing
  (->> (into (sorted-map) (merge unsolved-mystery m))
       (vals)
       (apply str)
       (println)))

(defn reduce-hashes [ans hash]
  (if-let [p (valid-pos? (nth hash 5) ans)]
    (let [c (nth hash 6)
          m (assoc ans p c)
          n (count m)]
      (print-answer m)
      (if (< 7 n) (reduced m) m))
    ans))

(->> (range) ;; solve part 2
     (pmap #(md5 (str "abbhdwsy" %)))
     (filter #(cs/starts-with? % "00000"))
     (reduce reduce-hashes {}))
