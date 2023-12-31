(ns advent-of-code.2023.4
  (:require [advent-of-code.elves :refer [day->input-lines]]
            [clojure.string :as cs]))

(def lines (day->input-lines 2023 4))

(defn parse-line [s]
  (let [[card numbers] (cs/split s #":")
        [winners hand] (cs/split numbers #"\|")
        read-numbers (fn [s]
                       (map parse-long (remove cs/blank? (cs/split s #"\s+"))))]
    [(parse-long (second (cs/split card #"\s+")))
     (read-numbers winners)
     (read-numbers hand)]))

(defn score [win-count]
  (if (pos? win-count)
    (last (take win-count (iterate #(* % 2) 1)))
    0))

;; part 1 solution
(apply +
       (for [[_ winners hand] (map parse-line lines)
             :let [win-count (count (clojure.set/intersection (set winners) (set hand)))]]
         (score win-count)))
