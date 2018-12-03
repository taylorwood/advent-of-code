(ns advent-of-code.2018.2
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as cs]
            [clojure.math.combinatorics :as comb]))

(def lines
  (->> (io/resource "data_2018/2.txt")
       (slurp)
       (cs/split-lines)))

;; solve part one
(defn score-box-id [id]
  (let [{has-2 2 has-3 3} (set/map-invert (frequencies id))]
    (cond
      (and has-2 has-3) :both
      has-2 :two
      has-3 :three)))
(let [{:keys [two three both]} (group-by score-box-id lines)]
  (* (+ (count two) (count both))
     (+ (count three) (count both))))

;; solve part two
(defn diff-indices [a b]
  (keep identity
        (map (fn [x y z] (when-not (= x y) z))
             a b (range))))
(first
 (for [[l r] (comb/combinations lines 2)
       :let [diff (diff-indices l r)]
       :when (= 1 (count diff))]
   (str (subs l 0 (first diff))
        (subs l (inc (first diff))))))


(comment
  "Render animated visualization solving part two; probably buggy."
  (require '[quil.middleware :as m]
           '[quil.core :as q])

  (def font-size 24)

  (defn replace-chars [s smap]
    (->> (mapcat identity smap)
         (apply assoc (vec s))
         (apply str)))

  (defn update-line-state [state]
    (if-let [[l r diff] (:curr-line state)]
      (let [mask (-> l
                     (replace-chars
                      (into {} (for [i (range (count l))
                                     :when (not (contains? (set diff) i))]
                                 [i " "]))))]
        (assoc state
               :left (replace-chars l (apply hash-map (interleave diff (repeat " "))))
               :mask mask
               :mask-trail (vec (take 20 (cons mask (:mask-trail state))))
               :right r
               :diff diff))
      state))

  (defn update-state [state]
    (cond-> state
      (not (:end? state))
      (assoc :lines (rest (:lines state))
             :curr-line (first (:lines state)))

      (not-empty (:lines state))
      (update-line-state)

      (let [[l r] (:curr-line state)
            diff (diff-indices l r)]
        (= 1 (count diff)))
      (assoc :match? true
             :end? true)

      (empty? (:lines state))
      (assoc :end? true)))

  (defn draw-state [state]
    (q/fill 0 40)
    (q/rect 0 0 (q/width) (q/height))
    (if (:match? state)
      (q/fill 255)
      (q/fill 234 207 76))
    (let [{:keys [left right mask mask-trail]} state
          left-y (- (q/height) (* 2 font-size))
          right-y (+ left-y font-size)]
      (when (and left right mask)
        ;; render left string
        (q/text left
                (- (/ (q/width) 2)
                   (/ (q/text-width left) 2))
                left-y)
        ;; render right string
        (q/text right
                (- (/ (q/width) 2)
                   (/ (q/text-width right) 2))
                right-y)
        ;; render differing characters in white
        (q/fill 255 0 0)
        (q/text mask
                (- (/ (q/width) 2)
                   (/ (q/text-width mask) 2))
                left-y)
        ;; render trail of previous mask diffs
        (dotimes [i (count mask-trail)]
          (q/fill (q/random 200 255) 0 0 (- 255 (* i (/ 255 (count mask-trail)))))
          (let [mask (nth mask-trail i)]
            (q/text mask
                    (- (/ (q/width) 2)
                       (/ (q/text-width mask) 2))
                    (- left-y
                       (* (inc i) font-size)))))))
    (when (or (:match? state) (:end? state))
      (q/no-loop)))

  (defn setup []
    (q/frame-rate 24)
    (q/background 0)
    (q/text-font (q/create-font "Courier New" font-size true))
    {:lines (for [[l r] (comb/combinations lines 2)
                  :let [diff (diff-indices l r)]]
              [l r diff])})

  (q/defsketch matrix
    :title "Inventory Management System"
    :size [500 500]
    :setup setup
    :update update-state
    :draw draw-state
    :features [:keep-on-top :resizable]
    :middleware [m/fun-mode]))
