(ns advent-of-code.2019.2
  (:require [advent-of-code.elves :refer :all]
            [clojure.string :refer [split]]))

(def values
  (mapv parse-long (split (first (day->input-lines 2019 2)) #",")))

;; solve part one
(def op-codes
  {1  +
   2  *
   99 :exit})

(defn run [program]
  (loop [program program
         pos 0]
    (let [op (op-codes (program pos))]
      (if (= :exit op)
        program
        (let [[pos-a pos-b pos-dest] (subvec program (inc pos) (inc (+ pos 4)))
              a (program pos-a)
              b (program pos-b)]
          (recur (assoc program pos-dest (op a b))
                 (+ pos 4)))))))

(run (assoc values 1 12, 2 2))

;; solve part two
(for [noun (range 0 (inc 99))
      verb (range 0 (inc 99))
      :let [output (-> values
                       (assoc 1 noun 2 verb)
                       (run))]
      :when (= 19690720 (first output))]
  (+ verb (* 100 noun)))
