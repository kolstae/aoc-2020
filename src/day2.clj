(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day2.txt")))

(defn parse-line [s]
  (let [[ns chr pwd] (str/split s #"[\s:]+")
        [from to] (map #(Integer/parseInt %) (str/split ns #"-"))]
    [from to (first chr) pwd]))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map parse-line)))

(defn valid-password [[from to chr pwd]]
  (let [cnt (get (frequencies pwd) chr 0)]
    (<= from cnt to)))

(defn part-1 [s]
  (count (filter valid-password (parse-input s))))

(comment
  (part-1 input))
;536

(defn char-at [s idx]
  (when (< idx (count s)) (.charAt s idx)))

(defn valid-password-2 [[idx1 idx2 chr pwd]]
  (let [ch1 (char-at pwd (dec idx1))
        ch2 (char-at pwd (dec idx2))]
    (or (and (= ch1 chr) (not= ch2 chr)) (and (not= ch1 chr) (= ch2 chr)))))

(defn part-2 [s]
  (count (filter valid-password-2 (parse-input s))))

(comment
  (part-2 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc")

  (part-2 input))
;558
