(ns advent-of-code.2017.18
  (:refer-clojure :exclude [set mod])
  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

(def raw-input (slurp (io/resource "data_2017/18.txt")))

(defn reg-val [registers register-or-value]
  (if (integer? register-or-value)
    register-or-value
    (get registers register-or-value 0)))

(defn inc-ip [registers] (update registers :ip inc))
;; getting cute with ~data->code~
(defn snd [registers x] (inc-ip (assoc registers :last-freq (reg-val registers x))))
(defn set [registers x y] (inc-ip (assoc registers x (reg-val registers y))))
(defn add [registers x y] (inc-ip (update registers x #(+ (or % 0) (reg-val registers y)))))
(defn mul [registers x y] (inc-ip (update registers x #(* (or % 0) (reg-val registers y)))))
(defn mod [registers x y] (inc-ip (update registers x #(clojure.core/mod (or % 0) (reg-val registers y)))))
(defn rcv [registers x]
  (if (= 0 (reg-val registers x))
    (inc-ip registers)
    (inc-ip (assoc registers :rcv-freq (registers :last-freq)))))
(defn jgz [registers x y]
  (if (pos? (reg-val registers x))
    (update registers :ip #(+ % (reg-val registers y)))
    (inc-ip registers)))

(defn parse-instruction [s]
  (let [[instr & args] (cs/split s #" ")
        args (map #(if (Character/isLetter (first %))
                     (keyword %)
                     (Integer/parseInt %))
                  args)]
    (apply list (symbol instr) args)))

(def instructions (mapv parse-instruction (cs/split-lines raw-input)))

;; solve part one
(loop [registers {:ip 0}]
  (when-let [[f & args] (nth instructions (:ip registers) nil)]
    (let [registers (apply (resolve f) registers args)]
      (or (:rcv-freq registers)
          (recur registers)))))

;; TODO solve part two
