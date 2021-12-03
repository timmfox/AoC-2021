(ns aoc2021.day3
  (:require [clojure.string :as str]))

(def sample ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])

#_(def input sample)
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

(def gamma-epsilon
  (->>
   (bit-frequencies input)
   (group-by ffirst)
   (into (sorted-map))
   (reduce (fn [[gamma epsilon] [x [[[_ lb] lv] [[_ rb] rv]]]]
             (if (> lv rv)
               [(str gamma lb) (str epsilon rb)]
               [(str gamma rb) (str epsilon lb)]))
           ["" ""])))

(defn bits->int
  [s]
  (apply + (map-indexed
            (fn [idx x] (* (int (Math/pow 2 idx)) x))
            (reverse (map #(Character/digit % 2) s)))))

(def part1 (apply * (map bits->int gamma-epsilon)))

(defn filter-readings
  [f ks c]
  (loop [coll c
         bit-idx 0]
    (if (<= (count coll) 1)
      (first coll)
      (let [[[_ filter-char]] (first
                               (sort-by second f (select-keys
                                                  (bit-frequencies coll)
                                                  (map (partial vector bit-idx) ks))))]
        (recur
         (filter #(= filter-char (get % bit-idx)) coll)
         (inc bit-idx))))))

(def part2 (apply * (map bits->int [(filter-readings > [\1 \0] input)
                                    (filter-readings < [\0 \1] input)])))
