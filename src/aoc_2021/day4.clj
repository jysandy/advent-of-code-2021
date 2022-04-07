(ns aoc-2021.day4
  (:require [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(comment
  ;; A bingo board looks like this:
  [[{:number  1
     :marked? false}
    {:number  2
     :marked? false}
    {:number  3
     :marked? false}
    {:number  4
     :marked? false}
    {:number  5
     :marked? false}]
   [{:number  1
     :marked? false}
    {:number  2
     :marked? false}
    {:number  3
     :marked? false}
    {:number  4
     :marked? false}
    {:number  5
     :marked? false}]
   ;; ... and so on. Five rows.
   ]
  )

(defn select-and-transform [pred transform-fn form]
  (walk/postwalk (fn [form]
                   (if (pred form)
                     (transform-fn form)
                     form))
                 form))

(defn mark-number [board number]
  (select-and-transform #(and (map? %)
                              (= number (:number %)))
                        #(assoc % :marked? true)
                        board))

(defn transpose [matrix]
  (apply mapv (fn [& args] (vec args)) matrix))

(defn row-marked? [row]
  (every? :marked? row))

(defn won? [board]
  (or (some row-marked? board)
      (some row-marked? (transpose board))))

(defn score [board number]
  (->> board
       (apply concat)
       (remove :marked?)
       (map :number)
       (reduce +)
       (* number)))

(defn play-bingo [boards numbers]
  (let [[winning-scores] (reduce (fn [[won-scores existing-boards] number]
                                   (let [marked-boards (map #(mark-number % number) existing-boards)
                                         won-board     (first (filter won? marked-boards))]
                                     (if (some? won-board)
                                       [(conj won-scores (score won-board number))
                                        (remove won? marked-boards)]
                                       [won-scores marked-boards])))
                                 [[] boards]
                                 numbers)]
    winning-scores))

(defn parse-int [s] (Integer/parseInt s))

(defn parse-board-from-string-rows [string-rows]
  (->> string-rows
       (select-and-transform string? string/trim)
       (map #(string/split % #"\s+"))
       (select-and-transform string? (fn [s]
                                       {:number  (parse-int s)
                                        :marked? false}))))

(defn read-numbers-and-boards []
  (let [input-lines   (->> (io/resource "day4input.txt")
                           io/reader
                           line-seq)
        bingo-numbers (map parse-int (-> input-lines
                                         first
                                         (string/split #",")))
        boards        (->> input-lines
                           rest
                           (partition-by string/blank?)
                           (remove (partial every? string/blank?))
                           (map parse-board-from-string-rows))]
    [bingo-numbers boards]))

(defn part1 []
  (let [[bingo-numbers boards] (read-numbers-and-boards)]
    (first (play-bingo boards bingo-numbers))))

;; ---------- part 2

(defn part2 []
  (let [[bingo-numbers boards] (read-numbers-and-boards)]
    (last (play-bingo boards bingo-numbers))))
