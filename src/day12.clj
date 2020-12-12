(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map (juxt first (comp #(Long/parseLong %) str/join next)))))

(def dirs [\W \N \E \S])

(defn do-next-op [{:keys [dir] :as state} [op d]]
  (case (if (= op \F) dir op)
    \N (update state :y - d)
    \S (update state :y + d)
    \W (update state :x - d)
    \E (update state :x + d)
    \L (assoc state :dir (->> dirs reverse cycle (drop-while #(not= % dir)) (drop (/ d 90)) first))
    \R (assoc state :dir (->> dirs cycle (drop-while #(not= % dir)) (drop (/ d 90)) first))))

(defn part-1 [s]
  (let [ops (parse-input s)
        {:keys [x y]} (reduce do-next-op {:x 0 :y 0 :dir \E} ops)]
    (+ (Math/abs ^long x) (Math/abs ^long y))))

(comment
  (part-1 "F10\nN3\nF7\nR90\nF11")

  (part-1 input))
;796

(defn rot-left [[x y]] [y (- x)])

(defn rot-right [[x y]] [(- y) x])

(defn rotate [wp f d]
  (->> wp
       (iterate f)
       (drop (/ d 90))
       first))

(defn move-wp [wp op d]
  (case op
    \N (update wp 1 - d)
    \S (update wp 1 + d)
    \W (update wp 0 - d)
    \E (update wp 0 + d)
    \L (rotate wp rot-left d)
    \R (rotate wp rot-right d)
    wp))

(defn move-ship [ship wp d]
  (mapv + ship (map #(* d %) wp)))

(defn next-move [{:keys [ship wp]} [op d]]
  {:ship (if (= \F op) (move-ship ship wp d) ship)
   :wp (move-wp wp op d)})

(defn part-2 [s]
  (let [ops (parse-input s)
        [x y] (:ship (reduce next-move {:ship [0 0] :wp [10 -1]} ops))]
    (+ (Math/abs ^long x) (Math/abs ^long y))))

(comment
  (part-2 "F10\nN3\nF7\nR90\nF11")

  (part-2 input))
;39446
