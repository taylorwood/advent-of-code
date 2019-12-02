(ns advent-of-code.2018.4
  (:require [advent-of-code.elves :refer :all]
            [clj-time.core :as time]
            [clj-time.format :as timef]
            [instaparse.core :as p]
            [instaparse.transform :as pt])
  (:import (org.joda.time Interval)))

(def lines (day->input-lines 2018 4))

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

(comment ;; viz
  (def naps
    "Map from guard IDs to maps of their sleep habits."
    (->> lines
         (map parse-entry)
         (sort-by first) ;; make chronological
         (reduce ;; reduce sequential events into sleep data
          (fn [[acc curr-guard nap-start] [timestamp [event new-guard]]]
            (case event
              :begin [acc new-guard]
              :sleep [acc curr-guard timestamp]
              :wake [(conj acc [curr-guard [nap-start timestamp]]) curr-guard]))
          [[]])
         (first)))

  (require '[quil.core :as q]
           '[quil.middleware :as m])

  (defn minutes-to-radians [minutes]
    (q/radians (* (/ 360 60) minutes)))

  (def font-size 14)

  (def time-formatter
    (timef/formatter "MMM dd YYYY HH:mm"))

  (defn draw-state [[[[guard [start end] [r g b]] & _events]
                     [min-heat max-heat]
                     hot-mins]]
    (if guard
      (let [clock-diameter (/ (q/width) 1.5)
            line-base-len (/ clock-diameter 2)
            hot-min-angles (map (fn [[minute heat]]
                                  [(minutes-to-radians minute) heat])
                                hot-mins)
            minute-angles (map minutes-to-radians
                               (range (time/minute start) (inc (time/minute end))))
            [min-ang max-ang] (apply (juxt min max) minute-angles)
            min-ang (+ min-ang (/ (q/radians (/ 360 60)) 2))
            max-ang (+ max-ang (/ (q/radians (/ 360 60)) 2))]
        (q/stroke-weight 1)
        (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
          ;; draw text
          (let [text (str "Guard #" guard " napped at " (timef/unparse time-formatter start))
                text-width (q/text-width text)]
            (q/with-fill 0
              (q/with-stroke 0
                (q/rect (- (/ (* text-width 4/3) 2))
                        (- (/ (q/height) 2.5) font-size)
                        (* text-width 4/3)
                        (* font-size 4/3))))
            (q/with-fill [r g b]
              (q/text text
                      (- (/ text-width 2))
                      (/ (q/height) 2.5))))
          ;; draw clock
          (q/with-rotation [(* -1 q/HALF-PI)]
                           ;; highlight hot spots
            (q/push-style)
            (q/blend-mode :screen)
            (q/color-mode :hsb 1)
            (doseq [[minute-angle heat] hot-min-angles
                    :let [hsb (when min-heat
                                (q/map-range heat min-heat max-heat 1 0))]]
              (q/stroke hsb 0.9 1)
              (q/stroke-weight (max 1 (q/map-range heat min-heat max-heat 1 5)))
              (q/with-rotation [minute-angle]
                (q/line 0 0 (q/map-range heat min-heat max-heat line-base-len (* 2 line-base-len)) 0)))
            (q/pop-style)
            ;; fill minute arcs
            (q/fill r g b 128)
            (q/stroke r 128 b 100)
            (doseq [minute-angle minute-angles]
              (q/with-rotation [minute-angle]
                (q/line 0 0 line-base-len 0)))
            (q/arc 0 0 clock-diameter clock-diameter min-ang max-ang)))
        (q/delay-frame 125))
      (q/no-loop)))

  (defn setup []
    (q/background 0)
    (q/frame-rate 60)
    (q/text-font (q/create-font "Monospaced" font-size))
    (let [guard-ids (set (map first naps))
          colors (into {} (for [gid guard-ids]
                            [gid [(q/random 255) (q/random 255) (q/random 255)]]))
          [min-heat max-heat] (->> (mapcat
                                    (fn [[_guard [start end] _rgb]]
                                      (range (time/minute start) (inc (time/minute end))))
                                    naps)
                                   (frequencies)
                                   (vals)
                                   (apply (juxt min max)))]
      (q/delay-frame 10000)
      [(map #(conj % (colors (first %))) naps)
       [min-heat max-heat]
       {}]))

  (defn update-state [state]
    (let [[[[_guard [start end] _rgb] & events] [min-heat max-heat] hot-mins] state]
      [(rest events)
       [min-heat max-heat]
       (merge-with (fnil + 0)
                   hot-mins
                   (into {} (for [m (range (time/minute start) (inc (time/minute end)))]
                              [m 1])))]))

  (q/defsketch clock
    :title "Nap Time"
    :size [600 600]
    :setup setup
    :draw draw-state
    :update update-state
    :middleware [m/fun-mode]
    :features [:keep-on-top :resizable]))
