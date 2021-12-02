(ns aoc2021.day2
  (:require [clojure.string :as str]))

;; https://adventofcode.com/2021/day/2

(def input (map
            #(hash-map
              :cmd (get % 1)
              :val (Integer/parseInt (get % 2)))
            (re-seq #"(\w+)\s(\d+)" (slurp "resources/day2.txt"))))

(defn travel1
  [x]
  (reduce (fn [{:keys [distance depth] :as acc} {:keys [cmd val]}]
            (condp = cmd
              "forward" (assoc acc :distance (+ distance val))
              "up"      (assoc acc :depth    (- depth val))
              "down"    (assoc acc :depth    (+ depth val))))
          {:distance 0 :depth 0}
          x))

(def part1 (let [destination (travel1 input)]
             (* (:distance destination) (:depth destination))))

(defn travel2
  [x]
  (reduce (fn [{:keys [distance depth aim] :as acc} {:keys [cmd val]}]
            (condp = cmd
              "forward" (assoc acc 
                               :distance (+ distance val)
                               :depth    (+ depth (* aim val)))
              "up"      (assoc acc :aim (- aim val))
              "down"    (assoc acc :aim (+ aim val))))
          {:distance 0 :depth 0 :aim 0}
          x))

(def part2 (let [destination (travel2 input)]
             (* (:distance destination) (:depth destination))))
