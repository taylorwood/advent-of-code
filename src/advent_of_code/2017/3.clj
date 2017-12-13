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


(def directions (cycle [:right :up :left :down]))

(defn move [pos dir]
  (case dir
    :right (update pos 0 inc)
    :up    (update pos 1 inc)
    :left  (update pos 0 dec)
    :down  (update pos 1 dec)))

(def neighbor-offsets
  [:right :up :left :down
   [:right :up] [:right :down]
   [:left :up] [:left :down]])

(defn neighbors [pos]
  (for [trans neighbor-offsets]
    (if (coll? trans)
      (reduce move pos trans)
      (move pos trans))))

(defn sum-neighbors [spiral pos]
  (reduce (fn [sum elem]
            (if-let [match (first (filter #(= % elem) spiral))]
              (+ sum (:value (meta match)))
              sum))
          0
          (neighbors pos)))

;; solve part two
;; cells are [X Y] coords with :value metadata
;; spiral works as a list or a set with cons, doesn't matter much for this input
(loop [spiral #{^{:value 1} [0 0]}
       dirs directions]
  (let [last-pos (first spiral)
        curr-pos (move last-pos (first dirs))
        neighbor-sum (sum-neighbors spiral curr-pos)
        ;; these calcs are irrelevant in terminal case but it reads nicer as one `let` :)
        next-pos (move curr-pos (second dirs))
        can-turn? (not-any? #{next-pos} spiral)
        spiral (cons (with-meta curr-pos {:value neighbor-sum}) spiral)]
    (if (< 347991 neighbor-sum)
      neighbor-sum
      (recur spiral (if can-turn? (rest dirs) dirs)))))
