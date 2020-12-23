(ns day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util ArrayDeque Collection HashSet)))

(set! *warn-on-reflection* true)

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

(defn compressed [as bs]
  (->> (concat as bs)
       (partition-all 10)
       (mapv (fn [ns] (reduce (fn [n x] (bit-or (bit-shift-left n 6) x)) ns)))))

(defn play-combat-2 [as bs]
  (let [as (ArrayDeque. ^Collection as) bs (ArrayDeque. ^Collection bs) seen (HashSet.)]
    (while (not (or (.isEmpty as) (.isEmpty bs)))
      (let [cs (compressed as bs)]
        (if (.add seen cs)
          (let [a (.pop as)
                b (.pop bs)]
            (if (if (and (<= a (.size as))
                         (<= b (.size bs)))
                  (first (play-combat-2 as bs))
                  (> a b))
              (do (.addLast as a) (.addLast as b))
              (do (.addLast bs b) (.addLast bs a))))
          (.clear as))))
    [(seq as) (seq bs)]))

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
;
