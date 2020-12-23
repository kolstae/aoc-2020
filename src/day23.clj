(ns day23
  (:require [clojure.string :as str]))

(def input "538914762")

(defn parse-input [s]
  (map #(- (int %) (int \0)) s))

(defn take-until [p s] (transduce (halt-when p (fn [r h] (conj r h))) conj [] s))

(defn do-turn [m]
  (fn [[c & ns]]
   (let [[ps ns] (split-at 3 ns)
         dest (first (remove (into #{0} ps) (iterate #(mod (dec %) m) (dec c))))
         head (take-until #{dest} ns)
         tail (drop (count head) ns)]
     ;(prn c ps :dest dest head tail)
     (concat head ps tail [c]))))

(defn part-1 [s]
  (->> (parse-input s)
       (iterate (do-turn 10))
       (drop 100)
       first
       cycle
       (drop-while (complement #{1}))
       next
       (take 8)
       str/join))

(comment
  (part-1 "389125467")

  (part-1 input))
;"54327968"

(defn part-2 [s n]
  (->> (concat (parse-input s) (range 10 1000001))
       (iterate (do-turn 1000001))
       (drop n)
       first
       cycle
       (drop-while (complement #{1}))
       next
       (take 2)
       (apply *)))

(comment
  (time (part-2 "389125467" 1000))

  (part-2 input 10000000))
;
