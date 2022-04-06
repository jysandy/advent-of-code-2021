(ns aoc-2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))



(comment (def initial-submarine-state {:position 0
                                       :depth    0})

         (defn move-forward [submarine-state amount]
           (update submarine-state :position + amount))

         (defn prevent-negative-depth [submarine-state]
           (if (neg? (:depth submarine-state))
             (assoc submarine-state :depth 0)
             submarine-state))

         (defn move-up [submarine-state amount]
           (prevent-negative-depth (update submarine-state :depth - amount)))

         (defn move-down [submarine-state amount]
           (update submarine-state :depth + amount))

         (defn process-instruction
           [submarine-state {:keys [command amount] :as _instruction}]
           (case command
             :forward (move-forward submarine-state amount)
             :up (move-up submarine-state amount)
             :down (move-down submarine-state amount)))

         (defn parse-instruction [instruction-string]
           (let [[command-string amount-string] (string/split instruction-string #" ")]
             {:command (keyword command-string)
              :amount  (Integer/parseInt amount-string)}))

         (def instructions (->> (io/resource "day2input.txt")
                                io/reader
                                line-seq
                                (map parse-instruction)))

         (defn day-2-part-1 []
           (let [{:keys [position depth]} (reduce process-instruction
                                                  initial-submarine-state
                                                  instructions)]
             (* position depth))))

;; -------------- part 2

(def initial-submarine-state {:position 0
                              :depth    0
                              :aim      0})

(defn prevent-negative-depth [submarine-state]
  (if (neg? (:depth submarine-state))
    (assoc submarine-state :depth 0)
    submarine-state))

(defn move-forward [{:keys [aim] :as submarine-state} amount]
  (prevent-negative-depth (-> submarine-state
                              (update :position + amount)
                              (update :depth + (* aim amount)))))

(defn move-up [submarine-state amount]
  (update submarine-state :aim - amount))

(defn move-down [submarine-state amount]
  (update submarine-state :aim + amount))

(defn process-instruction
  [submarine-state {:keys [command amount] :as _instruction}]
  (case command
    :forward (move-forward submarine-state amount)
    :up (move-up submarine-state amount)
    :down (move-down submarine-state amount)))

(defn parse-instruction [instruction-string]
  (let [[command-string amount-string] (string/split instruction-string #" ")]
    {:command (keyword command-string)
     :amount  (Integer/parseInt amount-string)}))

(def instructions (->> (io/resource "day2input.txt")
                       io/reader
                       line-seq
                       (map parse-instruction)))

(defn day-2-part-2 []
  (let [{:keys [position depth]} (reduce process-instruction
                                         initial-submarine-state
                                         instructions)]
    (* position depth)))