(ns aoc-2021.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn ocean-floor-grid [max-x max-y]
  (vec (repeat (inc max-y) (vec (repeat (inc max-x) 0)))))

(defn increment-at-point [grid [x y]]
  (update-in grid [y x] inc))

(defn zip-tuples [coll1 coll2]
  (map vector coll1 coll2))

(defn points-on-line [[x1 y1] [x2 y2]]
  (cond
    (= x1 x2) (zip-tuples (repeat x1)
                          (range (min y1 y2) (inc (max y1 y2))))
    (= y1 y2) (zip-tuples (range (min x1 x2) (inc (max x1 x2)))
                          (repeat y1))
    :else (zip-tuples (if (< x1 x2)
                        (range x1 (inc x2))
                        (range x1 (dec x2) -1))
                      (if (< y1 y2)
                        (range y1 (inc y2))
                        (range y1 (dec y2) -1)))))

(defn mark-line [grid [p1 p2]]
  (reduce increment-at-point grid (points-on-line p1 p2)))

(defn count-overlapping-points [grid lines]
  (let [marked-grid (reduce mark-line grid lines)]
    (->> marked-grid
         flatten
         (filter #(> % 1))
         count)))

(defn parse-int [s] (Integer/parseInt s))

(defn parse-point [point-string]
  (map parse-int (string/split point-string #",")))

(defn parse-line [line-string]
  (map parse-point (string/split line-string #" -> ")))

(defn not-diagonal? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2)
      (= y1 y2)))

(defn read-lines []
  (let [input-lines (->> (io/resource "day5input.txt")
                         io/reader
                         line-seq)]
    (map parse-line input-lines)))

(defn part1 []
  (let [lines      (->> (read-lines)
                        (filter not-diagonal?))
        all-points (apply concat lines)
        max-x      (apply max (map first all-points))
        max-y      (apply max (map second all-points))]
    (count-overlapping-points (ocean-floor-grid max-x max-y)
                              lines)))

(defn part2 []
  (let [lines      (read-lines)
        all-points (apply concat lines)
        max-x      (apply max (map first all-points))
        max-y      (apply max (map second all-points))]
    (count-overlapping-points (ocean-floor-grid max-x max-y)
                              lines)))