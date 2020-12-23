(ns day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (map (fn [s] (->> (str/split-lines s)
                    next
                    (mapv #(Long/parseLong %))))
       (str/split s #"\n\n+")))

(defn play-combat [as bs]
  (if (and (seq as) (seq bs))
    (let [[a & as] as
          [b & bs] bs]
      (if (< a b)
        (recur as (concat bs [b a]))
        (recur (concat as [a b]) bs)))
    [as bs]))

(defn part-1 [s]
  (let [[as bs] (parse-input s)]
    (->> (play-combat as bs)
         (mapcat identity)
         (reverse)
         (map * (map inc (range)))
         (reduce +))))

(comment
  (part-1 "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10")

  (part-1 input))
;34566

(defn play-combat-2 [as bs]
  (loop [as as bs bs seen #{}]
    (cond
      (contains? seen as) [as nil]
      (and (seq as) (seq bs)) (let [seen (conj seen as)
                                    [a & as] as
                                    [b & bs] bs]
                                (if (if (and (<= a (count as))
                                             (<= b (count bs)))
                                      (first (play-combat-2 (take a as) (take b bs)))
                                      (> a b))
                                  (recur (conj (vec as) a b) bs seen)
                                  (recur as (conj (vec bs) b a) seen)))
      :else [as bs])))

(defn part-2 [s]
  (let [[as bs] (parse-input s)]
    (->> (play-combat-2 as bs)
         (mapcat identity)
         (reverse)
         (map * (map inc (range)))
         (reduce +))))

(comment
  (part-2 "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10")

  (part-2 input))
;31854
