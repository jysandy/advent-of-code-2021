(ns aoc-2021.day8
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(defn digit [pattern]
  (get {2 1
        4 4
        3 7
        7 8}
       (count pattern)))

(defn parse-note [note-string]
  (let [[patterns output] (string/split note-string #"\s+\|\s+")]
    {:signals (map set (string/split patterns #"\s+"))
     :output  (map set (string/split output #"\s+"))}))

(defn part1 []
  (let [notes (->> (io/resource "day8input.txt")
                   io/reader
                   line-seq
                   (map parse-note))]
    (->> notes
         (mapcat :output)
         (map digit)
         (remove nil?)
         count)))

(def position-digit
  {#{1 2 3 5 6 7}   0
   #{3 6}           1
   #{1 3 4 5 7}     2
   #{1 3 4 6 7}     3
   #{2 3 4 6}       4
   #{1 2 4 6 7}     5
   #{1 2 4 5 6 7}   6
   #{1 3 6}         7
   #{1 2 3 4 5 6 7} 8
   #{1 2 3 4 6 7}   9})

(def possible-mappings (->> (combo/permutations "abcdefg")
                            (map (fn [permutation]
                                   (->> permutation
                                        (map-indexed (fn [index c]
                                                       [c (inc index)]))
                                        (into {}))))))

(defn render-digit [signal-mapping signal]
  (->> signal
       (map signal-mapping)
       set
       position-digit))

(defn valid-mapping? [signals signal-mapping]
  (let [rendered-signals (map (partial render-digit signal-mapping) signals)]
    (= [0 1 2 3 4 5 6 7 8 9]
       (sort rendered-signals))))

(defn signal-mapping [signals]
  (let [correct-mapping (->> possible-mappings
                             (filter (partial valid-mapping? signals))
                             first)]
    correct-mapping))

(defn parse-int [s] (Integer/parseInt s))

(defn displayed-number [signal-mapping digit-signals]
  (->> digit-signals
       (map (partial render-digit signal-mapping))
       (reduce str)
       parse-int
       ))

(defn part2 []
  (let [notes (->> (io/resource "day8input.txt")
                   io/reader
                   line-seq
                   (map parse-note))]
    (->> notes
         (map (fn [{:keys [signals output]}]
                (displayed-number (signal-mapping signals) output)))
         (reduce +))))