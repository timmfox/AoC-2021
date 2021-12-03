(ns aoc2021.day3
  (:require [clojure.string :as str]))

(def sample ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])

(def input
  (str/split-lines
   (slurp "resources/day3.txt")))

(defn str->idx-bits
  [s]
  (map-indexed vector s))

(defn bit-frequencies
  [coll]
  (frequencies
   (mapcat identity
           (map str->idx-bits coll))))

;; bits are provided in left to right, not in display right to left
(def gamma-epsilon
  (->>
   (bit-frequencies input)
   (group-by ffirst)
   (into (sorted-map-by (comp - compare)))
   (reduce (fn [[gamma epsilon] [_ [[[_ lb] lv] [[_ rb] rv]]]]
             (if (> lv rv)
               [(str gamma lb) (str epsilon rb)]
               [(str gamma rb) (str epsilon lb)]))
           ["" ""])))

(defn bits->int
  [s]
  (apply + (map-indexed
             (fn [idx x]
               (* (int (Math/pow 2 idx)) x))
             (map #(Character/digit % 2) s))))

(def part1 (apply * (map bits->int gamma-epsilon)))
