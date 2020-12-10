(ns day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (let [js (->> (str/split-lines s)
                (map (fn [s] (Long/parseLong s)))
                sort
                (cons 0))]
    (conj (vec js) (+ 3 (last js)))))


(defn part-1 [s]
  (let [js (parse-input s)
        fs (->> (partition 2 1 js)
                (map #(apply - (reverse %)))
                frequencies)]
    (* (get fs 1 0) (get fs 3 0))))

(comment
  (part-1 "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")
  (part-1 "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")

  (part-1 input))
;2277

(defn part-2 [s]
  (let [js (parse-input s)
        ds (->> (partition 2 1 js)
                (map #(apply - (reverse %))))
        ds (partition-by (fn [[d1 _ d2]] (max d1 d2)) (next (map vector (cons 1 ds) js ds)))]
    (->> ds
         (filter (fn [[[d1 _ d2]]] (= 1 (max d1 d2))))
         (map (comp {1 2 2 4 3 7} count))
         (apply *))))

(comment
  (part-2 "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")
  (part-2 "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")

  (part-2 input))
;37024595836928
