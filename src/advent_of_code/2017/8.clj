(ns advent-of-code.2017.8
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def raw-input (slurp (io/resource "data_2017/8.txt")))

(defn line-> [l]
  (let [[register op rhs _ regcomp comp val] (cs/split l #" ")
        op-val (Integer/parseInt rhs)
        update-op (case op "inc" + "dec" -)
        update-fn #(update-op (or % 0) op-val)
        compval (Integer/parseInt val)
        comp-fn (case comp
                  "==" =
                  "!=" not=
                  (ns-resolve 'clojure.core (symbol comp)))]
    {:predicate   #(comp-fn (or (get % regcomp) 0) compval)
     :instruction #(update % register update-fn)}))

(defn apply-instr [registers {:keys [predicate instruction]}]
  (if (predicate registers)
    (instruction registers)
    registers))

;; solve part one
(->> (cs/split-lines raw-input)
     (map line->)
     (reduce apply-instr {})
     (vals)
     (apply max))

;; solve part two
(->> (cs/split-lines raw-input)
     (map line->)
     (reductions apply-instr {})
     (mapcat vals)
     (apply max))
