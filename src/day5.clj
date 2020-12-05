(ns day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-ticket [s]
  (let [s (replace {\B \1 \R \1 \F \0 \L \0} s)
        row (Integer/parseInt (apply str (take 7 s)) 2)
        col (Integer/parseInt (apply str (drop 7 s)) 2)]
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
