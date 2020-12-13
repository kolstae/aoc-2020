(ns day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (let [[ts bs] (str/split-lines s)]
    [(Long/parseLong ts) (map #(if (not= "x" %) (Long/parseLong %) :x) (str/split bs #","))]))

(defn part-1 [s]
  (let [[ts bs] (parse-input s)
        [id m] (apply min-key second (map #(vector % (* % (inc (quot ts %)))) (filter number? bs)))]
    (* id (- m ts))))

(comment
  (part-1 "939\n7,13,x,x,59,x,31,19")

  (part-1 input))
;2045

(defn ext-gcd [a b]
  (loop [or a r b
         os 1 s 0]
    (if (pos? r)
      (recur r (mod or r)
             s (-' os (* (quot or r) s)))
      [or os])))

(defn combine-phase [a oa b ob]
  (let [[gcd s] (ext-gcd a b)
        phd (-' oa ob)]
    (when (zero? (mod phd gcd))
      (let [comb-p (*' (quot a gcd) b)]
        [comb-p (mod (-' oa (*' s (/ phd gcd) a)) comb-p)]))))

(defn part-2 [s]
  (->> (parse-input s)
       second
       (keep-indexed (fn [i x] (when (number? x) [i x])))
       (reduce (fn [[ia a] [ib b]]
                 (reverse (combine-phase a ia b (- ib)))))
       first))

(comment
  (part-2 "939\n7,13,x,x,59,x,31,19")
  (part-2 "1\n17,x,13,19")
  (part-2 "1\n67,7,59,61")
  (part-2 "1\n67,x,7,59,61")
  (part-2 "1\n67,7,x,59,61")
  (part-2 "1\n1789,37,47,1889")

  (part-2 input))
;402251700208309
