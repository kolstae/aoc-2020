(ns day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (str/split-lines s))

(defn infix-calc [[a f b & xs]]
  (if f
    (let [f (resolve f)]
      (if (next xs)
        (infix-calc (cons (f a b) xs))
        (f a b)))
    a))

(defn calc-line [l]
  (if-let [ss (seq (map (comp (juxt identity (comp infix-calc read-string)) first)
                        (re-seq #"\((\d+)(\s+([*+])\s+(\d+))+\)" l)))]
    (recur (reduce (fn [s [ss r]] (str/replace s ss (str r))) l ss))
    (infix-calc (read-string (str \( l \))))))

(defn part-1 [s]
  (->> (parse-input s)
       (map calc-line)
       (reduce +)))

(comment
  (part-1 "1 + (2 * 3) + (4 * (5 + 6))")
  (part-1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
  (part-1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
  (part-1 "1 + (2 * 3) + (4 * (5 + 6))")
  (part-1 "2 * 3 + (4 * 5)")

  (part-1 input))
;16332191652452

(defn calc-line2 [l]
  (if-let [ss (or (seq (map (comp (juxt identity (comp infix-calc read-string)) first)
                            (re-seq #"\((\d+)(\s+([+])\s+(\d+))+\)" l)))
                  (seq (map (comp (juxt identity (comp infix-calc read-string)) first)
                            (re-seq #"\((\d+)(\s+([*])\s+(\d+))+\)" l)))
                  (seq (map (comp (juxt identity (comp infix-calc read-string #(str \( % \)))) first)
                            (re-seq #"(\d+)(\s+([+])\s+(\d+))+" l))))]
    (recur (reduce (fn [s [ss r]] (str/replace-first s ss (str r))) l ss))
    (infix-calc (read-string (str \( l \))))))

(defn part-2 [s]
  (->> (parse-input s)
       (map calc-line2)
       (reduce +)))

(comment
  (part-2 "1 + 2 * 3 + 4 * 5 + 6")
  (part-2 "1 + (2 * 3) + (4 * (5 + 6))")
  (part-2 "2 * 3 + (4 * 5)")
  (part-2 "5 + (8 * 3 + 9 + 3 * 4 * 3)")
  (part-2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
  (part-2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

  (part-2 input))
;351175492232654
