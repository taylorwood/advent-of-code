(ns advent-of-code.2016.7
  (:require [clojure.string :as cs]))

(def raw-input (cs/split-lines (slurp "resources/advent/day-7.txt")))

(defn ip-parts [str] (re-seq #"[^\]]\w+[^\[]" str))

(defn ->part-type [str]
  (if (cs/starts-with? str "[") :hypernets :supernets))

(defn abba? [s]
  (->> (partition 4 1 s)
       (some (fn [[a b c d]]
               (and (= a d)
                    (= b c)
                    (not= a c))))))

(defn tls? [{:keys [supernets hypernets]}]
  (and (some abba? supernets)
       (not-any? abba? hypernets)))

(defn count-ips [pred]
  (->> (map ip-parts raw-input)
       (map #(group-by ->part-type %))
       (filter pred)
       (count)))

(count-ips tls?) ;; solve part 1

(defn aba? [s]
  (->> (partition 3 1 s)
       (filter (fn [[a b c]] (and (= a c) (not= a b))))
       (map #(apply str %))))

(defn ->bab [aba]
  (str (second aba) (first aba) (second aba)))

(defn ssl? [{:keys [supernets hypernets]}]
  (let [abas (mapcat aba? supernets)
        babs (map ->bab abas)
        bab? (fn [hn] (some #(cs/includes? hn %) babs))]
    (and (not-empty abas)
         (some bab? hypernets))))

(count-ips ssl?) ;; solve part 2
