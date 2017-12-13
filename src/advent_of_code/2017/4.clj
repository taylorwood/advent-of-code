(ns advent-of-code.2017.4
  (:require [clojure.string :as cs]
            [clojure.math.combinatorics :refer [combinations]]))

(def raw-input "too long")

(defn valid-passphrase? [phrase]
  (apply distinct? (cs/split phrase #" ")))

;; solve part one
(count (filter valid-passphrase? (cs/split-lines raw-input)))

(defn anagram? [[this that]]
  (= (frequencies this) (frequencies that)))

(defn valid-passphrase-too? [phrase]
  (let [words (cs/split phrase #" ")]
    (not (some anagram? (combinations words 2)))))

;; solve part two
(count (filter valid-passphrase-too? (cs/split-lines raw-input)))
