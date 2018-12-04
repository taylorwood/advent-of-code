(ns advent-of-code.2018.4
  (:require [advent-of-code.2018.3 :refer [update-keys]]
            [clj-time.core :as time]
            [clj-time.format :as timef]
            [clojure.string :as cs]
            [clojure.java.io :as io]
            [instaparse.core :as p]
            [instaparse.transform :as pt])
  (:import (org.joda.time Interval)))

(def lines
  (->> (io/resource "data_2018/4.txt")
       (slurp)
       (cs/split-lines)))

;; parse inputs

(def log-parser
  (p/parser "
    log = <'['> time <'] '> (begin | sleep | wake)
    time = num <'-'> num <'-'> num <' '> num <':'> num
    begin = <'Guard #'> num <' begins shift'>
    sleep = <'falls asleep'>
    wake = <'wakes up'>
    num = #'[0-9]+'
  "))

(defn parse-entry [entry]
  (->> entry
       (p/parse log-parser)
       (pt/transform {:num #(Integer/parseInt %) :time time/date-time})
       (rest)))

;; interpret inputs

(def hours-minutes-formatter (timef/formatter "HHmm"))
(defn interval->slept-minutes [^Interval interval]
  (let [start (.getStart interval)
        duration (time/in-minutes interval)]
    (for [i (range duration)]
      (->> (time/plus start (time/minutes i))
           (timef/unparse hours-minutes-formatter)))))

(def guard-sleep-study
  "Map from guard IDs to maps of their sleep habits."
  (->> lines
       (map parse-entry)
       (sort-by first) ;; make chronological
       (reduce ;; reduce sequential events into sleep data
        (fn [[curr-guard acc] [timestamp [event new-guard]]]
          (case event
            :begin [new-guard acc]
            :sleep [curr-guard (assoc-in acc [curr-guard :nap-start] timestamp)]
            :wake  (let [nap-start (get-in acc [curr-guard :nap-start])
                         nap-duration (time/interval nap-start timestamp)]
                     [curr-guard (-> acc
                                     (update-in [curr-guard :naps] conj nap-duration)
                                     (update curr-guard dissoc :nap-start))])))
        [nil {}])
       (second)
       (reduce-kv ;; aggregate per-guard sleep stats
        (fn [m guard {:keys [naps] :as sleep-data}]
          (assoc m guard (assoc sleep-data
                           :sleep-minutes (mapcat interval->slept-minutes naps))))
        {})))

;; solve part one
(let [[sleepiest-guard {:keys [sleep-minutes]}]
      (->> guard-sleep-study
           (sort-by (comp count :sleep-minutes second))
           (last))
      sleepiest-minute
      (->> sleep-minutes
           (frequencies)
           (apply max-key second)
           (key)
           (Integer/parseInt))]
  (* sleepiest-guard sleepiest-minute))

(defn update-vals
  "Returns m with all values applied to f with args."
  [m f & args]
  (persistent! (reduce-kv (fn [m k v] (assoc! m k (apply f v args))) (transient {}) m)))

(def time->guard-freqs
  "Map from times to guard sleep frequencies."
  (-> (reduce-kv
       (fn [m guard {:keys [sleep-minutes]}]
         (update-keys m sleep-minutes conj guard))
       {}
       guard-sleep-study)
      (update-vals frequencies)))

;; solve part two
(let [[most-slept-minute guard-freqs]
      (apply max-key
             #(apply max (vals (second %)))
             time->guard-freqs)
      most-freq-guard (key (apply max-key second guard-freqs))]
  (* most-freq-guard (Integer/parseInt most-slept-minute)))
