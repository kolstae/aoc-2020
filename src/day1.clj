(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map #(Integer/parseInt %))))

(defn pair-sum [sum ns]
  (loop [[n & ns] ns]
    (when (seq ns)
      (if-let [[x] (seq (filter #(= sum (+ n %)) ns))]
        [n x]
        (recur ns)))))

(defn part-1 [s]
  (apply * (pair-sum 2020 (parse-input s))))

(comment
  (part-1 input))
;605364

(defn triplet-sum [ns]
  (loop [[n & ns] ns]
    (if-let [[x y] (pair-sum (- 2020 n) ns)]
      [n x y]
      (recur ns))))

(defn part-2 [s]
  (apply * (triplet-sum (parse-input s))))

(comment
  (part-2 "1721\n979\n366\n299\n675\n1456")

  (part-2 input))
;128397680
