(ns aoc2021.day1
  (:require [clojure.string :as str]))

;; https://adventofcode.com/2021/day/1

(defn ->int
  [s]
  (Integer/parseInt s))

(def input
  (map ->int (-> (slurp "resources/day1.txt")
                 (str/split #"\n"))))

(defn count-depth-increases
  [{:keys [prev increases]} next-reading]
  {:prev next-reading
   :increases (cond
                (nil? prev) 0
                (< prev next-reading) (inc increases)
                :else increases)})

(def part-1
  (->> input
       (reduce count-depth-increases {})
       :increases))

(def sliding-windows
  (filter (comp (partial = 3) count)
          (loop [x (seq input)
                 y []]
            (if x
              (recur (seq (rest x)) (conj y (take 3 x)))
              y))))

(def part-2
  (->> (map (partial apply +) sliding-windows)
       (reduce count-depth-increases {})
       :increases))
