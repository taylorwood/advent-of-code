(ns advent-of-code.2017.17)

(def stride 355)

(defn insert [buf i n]
  (let [[pre post] (split-at i buf)]
    (concat pre [n] post)))

;; solve part one
(->> (reduce
       (fn [{:keys [curr-pos buffer]} elem]
         (let [new-pos (inc (mod (+ curr-pos stride)
                                 (count buffer)))]
           {:curr-pos new-pos
            :buffer   (insert buffer new-pos elem)}))
       {:curr-pos 0
        :buffer   [0]}
       (range 1 2018))
     :buffer
     (drop-while #(not= 2017 %))
     (second))

;; solve part two:
;; building the buffer is too slow for 50M iterations
;; and we only need to keep track of values that are
;; inserted right after zero, which will always be the
;; first item in buffer
(reduce
  (fn [{:keys [curr-pos len post-zero]} elem]
    (let [new-pos (inc (mod (+ curr-pos stride) len))]
      {:curr-pos  new-pos
       :len       (inc len)
       :post-zero (if (= 1 new-pos) elem post-zero)}))
  {:curr-pos 0
   :len      1}
  (range 1 50000000))
