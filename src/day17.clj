(ns day17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map-indexed (fn [y l] (map-indexed #(vector [%1 y] %2) l)))
       (mapcat identity)
       (filter (comp #{\#} second))
       (map first)))

(defn next-state-3 [st n-st p]
  (let [[x y z] p
        cnt (->> (for [x (range (dec x) (+ 2 x)) y (range (dec y) (+ 2 y)) z (range (dec z) (+ 2 z))
                       :let [pp [x y z]]
                       :when (and (not= pp p) (contains? st pp))] pp)
                 (take 4)
                 (count))]
    (if (contains? st p)
      (if (<= 2 cnt 3) n-st (disj n-st p))
      (if (= cnt 3) (conj n-st p) n-st))))

(defn do-cycle-3 [state]
  (let [[min-p max-p] (reduce (fn [[min-p max-p] p] [(map min min-p p) (map max max-p p)])
                              [(repeat 3 Double/POSITIVE_INFINITY) (repeat 3 Double/NEGATIVE_INFINITY)]
                              state)
        [min-x min-y min-z] (map dec min-p)
        [max-x max-y max-z] (map inc max-p)]
    (reduce #(next-state-3 state %1 %2)
            state
            (for [x (range min-x (inc max-x)) y (range min-y (inc max-y)) z (range min-z (inc max-z))] [x y z]))))

(defn part-1 [s]
  (->> (parse-input s)
       (into #{} (map #(conj % 0)))
       (iterate do-cycle-3)
       (drop 6)
       first
       count))

(comment
  (part-1 ".#.\n..#\n###")

  (part-1 input))
;362

(defn next-state-4 [st n-st p]
  (let [[x y z w] p
        cnt (->> (for [x (range (dec x) (+ 2 x)) y (range (dec y) (+ 2 y)) z (range (dec z) (+ 2 z)) w (range (dec w) (+ 2 w))
                       :let [pp [x y z w]]
                       :when (and (not= pp p) (contains? st pp))] pp)
                 (take 4)
                 (count))]
    (if (contains? st p)
      (if (<= 2 cnt 3) n-st (disj n-st p))
      (if (= cnt 3) (conj n-st p) n-st))))

(defn do-cycle-4 [state]
  (let [[min-p max-p] (reduce (fn [[min-p max-p] p] [(map min min-p p) (map max max-p p)])
                              [(repeat 4 Double/POSITIVE_INFINITY) (repeat 4 Double/NEGATIVE_INFINITY)]
                              state)
        [min-x min-y min-z min-w] (map dec min-p)
        [max-x max-y max-z max-w] (map inc max-p)]
    (reduce #(next-state-4 state %1 %2)
            state
            (for [x (range min-x (inc max-x)) y (range min-y (inc max-y)) z (range min-z (inc max-z)) w (range min-w (inc max-w))]
              [x y z w]))))


(defn part-2 [s]
  (->> (parse-input s)
       (into #{} (map #(conj % 0 0)))
       (iterate do-cycle-4)
       (drop 6)
       first
       count))

(comment
  (part-2 ".#.\n..#\n###")

  (part-2 input))
;1980
