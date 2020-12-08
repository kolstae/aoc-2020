(ns day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map (fn [s] (let [[op arg] (str/split s #"\s")] [(keyword op) (Integer/parseInt arg)])))))

(defmulti op ffirst)

(defmethod op :nop [[_ state]] state)

(defmethod op :acc [[[_ arg] state]]
  (update state :acc + arg))

(defmethod op :jmp [[[_ arg] state]]
  (update state :ptr + (dec arg)))

(defn part-1 [s]
  (loop [state {:ptr 0 :acc 0} ops (vec (parse-input s)) done #{}]
    #_(prn state)
    (let [ptr (:ptr state)]
      (if (contains? done ptr)
        state
        (recur (update (op [(get ops ptr) state]) :ptr inc) ops (conj done ptr))))))

(comment
  (part-1 "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6")

  (part-1 input))
;1723

(defn run-ops [ops]
  (loop [state {:ptr 0 :acc 0} ops ops done #{}]
    (let [ptr (:ptr state)]
      (cond
        (contains? done ptr) nil
        (= ptr (count ops)) (:acc state)
        true (recur (update (op [(get ops ptr) state]) :ptr inc) ops (conj done ptr))))))

(defn part-2 [s]
  (let [ops (vec (parse-input s))]
    (->> ops
         (keep-indexed (fn [i [op]] (when-let [rop (get {:nop :jmp :jmp :nop} op)] [i rop])))
         (map (fn [[i rop]] (assoc-in ops [i 0] rop)))
         (keep run-ops)
         (first))))

(comment
  (part-2 "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6")

  (part-2 input))
;846
