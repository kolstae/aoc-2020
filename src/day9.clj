(ns day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map (fn [s] (Long/parseLong s)))))

(defn pair-sum [sum ns]
  (loop [[n & ns] ns]
    (if (or (empty? ns) (seq (filter #(= sum (+ n %)) ns)))
      (empty? ns)
      (recur ns))))

(defn part-1 [preamble s]
  (let [ns (parse-input s)]
    (->> (map (fn [n ns] (when (pair-sum (nth ns n) (take n ns)) (nth ns n)))
              (repeat (- (count ns) preamble) preamble)
              (iterate next ns))
         (drop-while nil?)
         (first))))

(comment
  (part-1 5 "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576")

  (part-1 25 input))
;400480901

(defn contiguous-sum [sum ns]
  (loop [[n & ns] ns res-ns [] rest-sum sum]
    (when (seq ns)
      (let [rest-sum (- rest-sum n)
            res-ns (conj res-ns n)]
        (cond
          (neg? rest-sum) nil
          (zero? rest-sum) [(apply min res-ns) (apply max res-ns)]
          :else (recur ns res-ns rest-sum))))))

(defn part-2 [wanted s]
  (let [ns (parse-input s)]
    (->> (iterate next ns)
         (take-while seq)
         (keep (fn [ns] (contiguous-sum wanted ns)))
         (first)
         (apply +))))

(comment
  (part-2 127 "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576")

  (part-2 400480901 input))
;67587168
