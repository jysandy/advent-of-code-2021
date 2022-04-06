(ns aoc-2021.day1
  (:require [clojure.java.io :as io]))

(defn parse-int [s] (Integer/parseInt s))

(def input-numbers (->> (io/resource "day1input.txt")
                        io/reader
                        line-seq
                        (map parse-int)))

(defn adjacent-element-pairs [v]
  (map (fn [a b] [a b])
       v
       (drop 1 v)))

(defn increasing-pair? [[a b]] (< a b))

(defn day-1-part-1 []
  (->> input-numbers
       adjacent-element-pairs
       (filter increasing-pair?)
       count))

(defn sliding-windows-of-3 [v]
  (map (fn [& args] (vec args))
       v
       (drop 1 v)
       (drop 2 v)))

(defn sum [v] (reduce + v))

(defn day-2-part-2 []
  (->> input-numbers
       sliding-windows-of-3
       (map sum)
       adjacent-element-pairs
       (filter increasing-pair?)
       count))

