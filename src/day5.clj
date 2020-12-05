(ns day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn bin-str [u-ch max cs]
  (first (reduce (fn [[min max] ch]
                   (let [d (/ (- max min) 2)]
                     (if (= ch u-ch)
                       [(+ min d) max]
                       [min (- max d)])))
                 [0 max]
                 cs)))

(defn parse-ticket [s]
  (let [row (bin-str \B 128 (take 7 s))
        col (bin-str \R 8 (drop 7 s))]
    [row col (+ (* 8 row) col)]))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map parse-ticket)))

(defn part-1 [s]
  (apply max (map last (parse-input s))))

(comment
  (parse-input "BFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL")

  (part-1 input))
;908

(defn part-2 [s]
  (->> (parse-input s)
       sort
       (partition 2 1)
       (filter (fn [[s1 s2]] (= 2 (- (last s2) (last s1)))))
       ffirst
       last
       inc))

(comment
  (part-2 input))
;147
