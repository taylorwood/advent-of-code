(ns advent-of-code.2019.5
  (:require [advent-of-code.elves :refer :all]))

(def values
  (mapv parse-long (clojure.string/split (first (day->input-lines 2019 5)) #",")))

(def mode->get-fn
  {0 (fn [program pos] (program pos))
   1 (fn [_program pos] pos)})

(defn parse-opcode [n]
  (let [[mode-3 mode-2 mode-1 op-a op-b] (take-last 5 (concat (repeat 4 0) (digits n)))
        modes [(mode->get-fn mode-1)
               (mode->get-fn mode-2)
               (mode->get-fn mode-3)]]
    {:modes modes
     :op (cond
           (= 1 op-b) :add
           (= 2 op-b) :mul
           (= 3 op-b) :put
           (= 4 op-b) :prn
           (= 5 op-b) :jump-if-true
           (= 6 op-b) :jump-if-false
           (= 7 op-b) :less-than
           (= 8 op-b) :equals
           (= [9 9] [op-a op-b]) :exit)}))

(def arg-count {:add 3 :mul 3 :put 1 :prn 1 :less-than 3 :equals 3 :jump-if-true 2 :jump-if-false 2 :exit 0})

(declare input)

(defn parse-instruction [program pos {:keys [op modes]}]
  (let [[f1 f2 _f3] modes
        [a1 a2 a3] (subvec program
                           (inc pos)
                           (+ (inc pos) (arg-count op)))
        new-pos (inc (+ pos (arg-count op)))]
    (case op
      :add
      (-> program
          (assoc a3 (+ (f1 program a1) (f2 program a2)))
          (vary-meta assoc :pos new-pos))
      :mul
      (-> program
          (assoc a3 (* (f1 program a1) (f2 program a2)))
          (vary-meta assoc :pos new-pos))
      :put
      (-> program
          (assoc a1 input)
          (vary-meta assoc :pos new-pos))
      :prn
      (do
        (prn (program a1))
        (vary-meta program assoc :pos new-pos))
      :jump-if-true
      (vary-meta program assoc :pos (if-not (zero? (f1 program a1))
                                      (f2 program a2)
                                      new-pos))
      :jump-if-false
      (vary-meta program assoc :pos (if (zero? (f1 program a1))
                                      (f2 program a2)
                                      new-pos))
      :less-than
      (-> program
          (assoc a3 (if (< (f1 program a1) (f2 program a2)) 1 0))
          (vary-meta assoc :pos new-pos))
      :equals
      (-> program
          (assoc a3 (if (= (f1 program a1) (f2 program a2)) 1 0))
          (vary-meta assoc :pos new-pos))
      :exit
      program)))

(def input 5) ;; change for day 1 vs 2

(loop [pos 0
       program values]
  (let [opcode (parse-opcode (program pos))]
    (when-not (= :exit (:op opcode))
      (let [program (parse-instruction program pos opcode)]
        (recur (:pos (meta program))
               program)))))
