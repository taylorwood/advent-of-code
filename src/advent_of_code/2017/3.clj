(ns advent-of-code.2017.3)

;; this pattern became apparent after drawing out a larger spiral
;; and I'm sure there must be a better impl. for this predicate...
(defn odd-perfect-square? [n]
  (and (odd? n)
       (= (Math/floor (Math/sqrt n))
          (Math/sqrt n))))

;; once we know what spiral level we're on, we need to know how far around the spiral we are
;; hacky iterative regression follows...
(defn steps-from-bottom-right [n]
  (loop [steps 0 n n]
    (cond
      (odd-perfect-square? n) steps
      (pos? n) (recur (inc steps) (dec n)))))

;; solving this with arithmetic was really challenging for me
;; and took several hours of futzing in the REPL, printing out
;; tables of numbers! A naive/mechanical spiral-building approach
;; seemed most obvious, but I felt there must be a math solution:
(defn spiral-manhattan-dist [n]
  (if (= 1 n)
    0
    (let [wraps (Math/ceil (- (Math/sqrt (/ n 4)) 0.5))     ;; how many times has the spiral wrapped?
          len (inc (* wraps 2))                             ;; how many steps does each side of a spiral have?
          br-dist (steps-from-bottom-right n)               ;; how far are we from the spiral head?
          mid-index (Math/floor (/ len 2))                  ;; what index would the middle cell occupy?
          corner-dist (mod br-dist (dec len))]              ;; how many steps are we from the prev corner?
      (+ wraps                                              ;; the answer is the number of wraps in the spiral plus...
         (Math/abs (- mid-index corner-dist))))))           ;; how many steps off-center (X *or* Y axis) we are

(spiral-manhattan-dist 347991)                              ;; solve part one
