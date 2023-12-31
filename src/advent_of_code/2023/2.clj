(ns advent-of-code.2023.2
  (:require [advent-of-code.elves :refer [day->input-lines]]
            [clojure.string :as cs]))

(defn subset->counts [s]
  (let [sets (cs/split s #",\s+")]
    (into {} (for [set sets
                   :let [[num color] (cs/split set #" ")]]
               [(keyword color) (parse-long num)]))))

(defn parse-line [s]
  (let [[game-pre subsets] (cs/split s #":\s+")
        subsets (cs/split subsets #";\s+")]
    {:game (parse-long (second (cs/split game-pre #" ")))
     :subsets (for [sub subsets]
                (subset->counts sub))}))

(def games
  (for [l (day->input-lines 2023 2)]
    (-> (parse-line l)
        (update :subsets #(apply merge-with max %)))))

(def possible-games
  (filter #(and (>= 12 (get-in % [:subsets :red]))
                (>= 13 (get-in % [:subsets :green]))
                (>= 14 (get-in % [:subsets :blue])))
          games))

;; part 1 solution
(apply + (map :game possible-games))

(def powers
  (for [{:keys [subsets]} games]
    (* (:red subsets) (:green subsets) (:blue subsets))))

;; part 2 solution
(apply + powers)
