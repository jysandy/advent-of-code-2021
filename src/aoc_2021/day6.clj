(ns aoc-2021.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def initial-state {0 0
                    1 0
                    2 0
                    3 0
                    4 0
                    5 0
                    6 0
                    7 0
                    8 0})

(defn call-repeatedly [f n x]
  (if (<= n 0)
    x
    (recur f (dec n) (f x))))

(defn breed-fish [fishies]
  (let [breedable-fish-count (get fishies 0)]
    (-> (reduce (fn [fishies number]
                  (-> fishies
                      (update (dec number) + (get fishies number))
                      (assoc number 0)))
                (assoc fishies 0 0)
                (range 1 9))
        (update 6 + breedable-fish-count)
        (update 8 + breedable-fish-count))))

(defn parse-int [s] (Integer/parseInt s))

(defn part1 []
  (let [fishies-list (map parse-int (-> (io/resource "day6input.txt")
                                        slurp
                                        string/trim
                                        (string/split #",")))
        fishies      (reduce (fn [fishies [fish-num fish-count]]
                               (update fishies fish-num + fish-count))
                             initial-state
                             (frequencies fishies-list))]
    (reduce + (vals (call-repeatedly breed-fish 80 fishies)))))

(defn part2 []
  (let [fishies-list (map parse-int (-> (io/resource "day6input.txt")
                                        slurp
                                        string/trim
                                        (string/split #",")))
        fishies      (reduce (fn [fishies [fish-num fish-count]]
                               (update fishies fish-num + fish-count))
                             initial-state
                             (frequencies fishies-list))]
    (reduce + (vals (call-repeatedly breed-fish 256 fishies)))))