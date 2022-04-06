(ns aoc-2021.day3
  (:require [clojure.java.io :as io]))

(defn parse-binary-string [s]
  (Integer/parseInt s 2))

(defn add-seq-to-item-lists [item-lists s]
  (map conj item-lists s))

(defn build-bit-lists [bit-strings]
  (let [starting-list (repeat (count (first bit-strings))
                              [])]
    (reduce add-seq-to-item-lists starting-list bit-strings)))

(defn most-frequent [coll]
  (->> coll
       frequencies
       (sort-by second >)
       first
       first))

(defn least-frequent [coll]
  (->> coll
       frequencies
       (sort-by second <)
       first
       first))

(defn gamma-rate [bit-lists]
  (->> bit-lists
       (map most-frequent)
       (reduce str)
       parse-binary-string))

(defn epsilon-rate [bit-lists]
  (->> bit-lists
       (map least-frequent)
       (reduce str)
       parse-binary-string))

(defn day-3-part-1 []
  (let [bit-lists (->> (io/resource "day3input.txt")
                       io/reader
                       line-seq
                       build-bit-lists)]
    (* (gamma-rate bit-lists)
       (epsilon-rate bit-lists))))

;; -------------- part 2

(defn gas-rating [bit-criterion numbers]
  (loop [filtered-numbers numbers
         bit-index        0]
    (if (or (= 1 (count filtered-numbers))
            (= (count (first numbers)) bit-index))
      (-> filtered-numbers
          first
          parse-binary-string)
      (let [bit-to-match (bit-criterion filtered-numbers bit-index)]
        (recur (filter #(= bit-to-match (nth % bit-index))
                       filtered-numbers)
               (inc bit-index))))))

(defn most-frequent-bit-or-1 [bits]
  (let [freq-dist (frequencies bits)]
    (if (reduce = (vals freq-dist))
      \1
      (most-frequent bits))))

(defn least-frequent-bit-or-0 [bits]
  (let [freq-dist (frequencies bits)]
    (if (reduce = (vals freq-dist))
      \0
      (least-frequent bits))))

(defn oxygen-bit-criterion [numbers bit-index]
  (->> numbers
       (map #(nth % bit-index))
       most-frequent-bit-or-1))

(defn co2-bit-criterion [numbers bit-index]
  (->> numbers
       (map #(nth % bit-index))
       least-frequent-bit-or-0))

(defn day-3-part-2 []
  (let [numbers                 (->> (io/resource "day3input.txt")
                                     io/reader
                                     line-seq)
        oxygen-generator-rating (gas-rating oxygen-bit-criterion numbers)
        co2-scrubber-rating     (gas-rating co2-bit-criterion numbers)]
    (* oxygen-generator-rating co2-scrubber-rating)))

