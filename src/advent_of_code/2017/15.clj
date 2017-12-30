(ns advent-of-code.2017.15)

(def generators {:a {:seed 679 :factor 16807}
                 :b {:seed 771 :factor 48271}})

(defn generator-seq [{:keys [seed factor]} xform]
  (->> (iterate #(rem (* factor %) 2147483647) seed)
       (rest)
       (eduction xform)))

(def lower-16 (map (partial bit-and 0xffff)))

;; solve part one
(->> (map vector
          (generator-seq (generators :a) lower-16)
          (generator-seq (generators :b) lower-16))
     (take 4e7)
     (filter (partial apply =))
     (count))

;; solve part two
(->> (map vector
          (generator-seq (generators :a)
                         (comp lower-16 (filter #(= 0 (mod % 4)))))
          (generator-seq (generators :b)
                         (comp lower-16 (filter #(= 0 (mod % 8))))))
     (take 5e6)
     (filter (partial apply =))
     (count))
