(ns day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-rule [s]
  (let [[k & rs] (->> (str/split s #"\s*(contain|,)\s*")
                      (map (fn [s] (let [[_ no color] (re-matches #"(\d)?\s*(\w+ \w+).*" s)]
                                     (if no
                                       [color (Integer/parseInt no)]
                                       color)))))]
    [k rs]))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map parse-rule)))

(defn part-1 [s]
  (let [rs (parse-input s)]
    (loop [wanted #{"shiny gold"} can-hold #{}]
      (let [more (seq (keep (fn [[k rs]] (when (and (not (contains? wanted k))
                                                    (seq (filter (comp wanted first) rs)))
                                           k)) rs))]
        (if more
          (recur (into wanted more) (into can-hold more))
          (count can-hold))))))

(comment
  (part-1 "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.")

  (part-1 input))
;148

(defn part-2 [s]
  (let [rs (into {} (filter (comp vector? first second)) (parse-input s))]
    (loop [wanted {"shiny gold" 1} bags 0]
      (if-let [more (seq (mapcat (fn [[k n]] (map #(update % 1 * n) (rs k))) wanted))]
        (recur more (reduce + bags (map second more)))
        bags))))

(comment
  (part-2 "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.")
  (part-2 "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags.")

  (part-2 input))
;24867
