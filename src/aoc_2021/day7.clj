(ns aoc-2021.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def sample [16, 1, 2, 0, 4, 2, 7, 1, 2, 14])

(defn sum [coll] (reduce + coll))

(defn simple-cost [crab position]
  (Math/abs ^Integer (- crab position)))

(defn ap-sum [n]
  (* (/ n 2) (+ 2 (dec n))))

(defn progressive-cost [crab position]
  (ap-sum (simple-cost crab position)))

(defn fuel-cost [cost-fn crabs position]
  (sum (map #(cost-fn % position)
            crabs)))

(defn minimum-fuel-cost [cost-fn crabs]
  (apply min
         (map (partial fuel-cost cost-fn crabs)
              (range (apply min crabs) (inc (apply max crabs))))))

(defn parse-int [s] (Integer/parseInt s))

(defn part1 []
  (let [crabs (map parse-int (-> (io/resource "day7input.txt")
                                 slurp
                                 string/trim
                                 (string/split #",")))]
    (minimum-fuel-cost simple-cost crabs)))

(defn part2 []
  (let [crabs (map parse-int (-> (io/resource "day7input.txt")
                                 slurp
                                 string/trim
                                 (string/split #",")))]
    (minimum-fuel-cost progressive-cost crabs)))