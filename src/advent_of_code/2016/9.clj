(ns advent-of-code.2016.9)

(def raw-input (slurp "resources/advent/day-9.txt"))

(defn parse-cmd [s]
  (->> (re-find #"(\d+)x(\d+)" s)
       (drop 1)
       (map #(Integer/parseInt %))))

(defn decompress [s & [recursive]]
  (-> (loop [in s
             st {:decoded   0        ;; count "decompressed" chars
                 :state     :content ;; current parse state
                 :cmd-stack []}]     ;; buffer parenthesized command chars
        (if (empty? in)
          st
          (let [{:keys [decoded cmd-stack state]} st
                c (first in)
                p (case c
                    \( :open-paren
                    \) :close-paren
                    state)
                [cc rl] (if (= :close-paren p)
                          (parse-cmd (apply str cmd-stack)))
                s (if (and cc rl) ;; apply parsed cmd
                    (assoc st :state     :content
                              :decoded   (+ decoded
                                            (if recursive ;; pre-process inner cmds
                                              (* rl (decompress (->> (drop 1 in) ;; hack :)
                                                                     (take cc)) recursive))
                                              (* rl cc)))
                              :cmd-stack [])
                    (assoc st :state     p ;; else append content char-by-char
                              :decoded   (if (= :content p)
                                           (inc decoded)
                                           decoded)
                              :cmd-stack (if (not= :content p)
                                           (conj (or cmd-stack []) c))))
                rem (if cc
                      (drop cc (rest in))
                      (rest in))]
            (recur rem s))))
      (:decoded)))

(decompress raw-input)            ;; solve part 1
(decompress raw-input :recursive) ;; solve part 2
