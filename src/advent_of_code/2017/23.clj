(ns advent-of-code.2017.23
  (:refer-clojure :exclude [set])
  (:require [advent-of-code.2017.18 :refer [parse-instruction inc-ip reg-val]]
            [clojure.java.io :as io]
            [clojure.string :as cs]))

(def raw-input (slurp (io/resource "data_2017/23.txt")))

(def instructions (->> (cs/split-lines raw-input)
                       (mapv parse-instruction)))

(defn set [registers x y] (inc-ip (assoc registers x (reg-val registers y))))
(defn mul [registers x y] (inc-ip
                            (-> registers
                                (update x #(* (or % 0) (reg-val registers y)))
                                (update :muls (fnil inc 0)))))
(defn sub [registers x y] (inc-ip (update registers x #(- (or % 0) (reg-val registers y)))))
(defn jnz [registers x y]
  (if-not (zero? (reg-val registers x))
    (update registers :ip #(+ % (reg-val registers y)))
    (inc-ip registers)))

;; solve part one
(loop [registers {:ip 0}]
  (if-let [[f & args :as instr] (nth instructions (:ip registers) nil)]
    (let [registers (apply (resolve f) registers args)]
      (recur registers))
    registers))

;; solving part two

;; I did some printf tracing of the program and noticed the loops
;; then went about looking at the instruction branches, then assumed
;; an easy approach would be to reassemble the instructions...
; b = 93;
; c = b
; b *= 100;
; b += 100000;
; c = b;
; c += 17000;
;
; while(true):
;
;  f = false;
;  for (d = 2; d != b; d++) {
;      for (e = 2; e != b; e++) {
;          if (d * e == b) { // checking for primes
;            f = true;
;            break; // NOTE this is an optimization/missing instruction
;          }
;      }
;  }
;
;  if (f == true) {
;    h++;
;
;  if (b == c)
;      return h;
;  b += 17;

(def b (+ 100000 (* 93 100)))
(def c (+ b 17000))

(comment
  "Clojurized version but still slow due to... inefficient prime finding routine!"
  (loop [b b
         h 0]
    (let [h (if (->> (for [x (range 2 (inc b))
                           y (range 2 (inc b))]
                       (* x y))
                     (some #(= b %)))
              (inc h)
              h)]
      (if (= b c)
        h
        (recur (+ b 17) h)))))

(defn is-prime? [x]
  (and (> x 2)
       (loop [i 2
              max-comp (int (Math/sqrt x))]
         (or (> i max-comp)
             (if (= 0 (mod x i))
               false
               (recur (inc i) max-comp))))))

;; the most ~beautiful~ solution
(->> (range b (inc c) 17)
     (filter (comp not is-prime?))
     (count))
