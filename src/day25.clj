(ns day25
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (map #(Long/parseLong %) (str/split-lines s)))

(defn trans [sn]
  (fn [[i n]]
    [(inc i) (mod (* n sn) 20201227)]))

(defn part-1 [s]
  (let [ks (set (parse-input s))
        [[cls _] [_ dpk]] (take 2 (filter (comp ks second) (iterate (trans 7) [0 1])))]
    (second (first (drop cls (iterate (trans dpk) [0 1]))))))

(comment
  (part-1 "5764801\n17807724")

  (part-1 input))
;8329514

(defn part-2 [s]
  (parse-input s))

(comment
  (part-2 "5764801\n17807724")

  (part-2 input))
;
