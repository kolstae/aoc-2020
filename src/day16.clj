(ns day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-rule [s]
  (let [[name rs] (str/split s #":\s*")
        rs (str/split rs #"\s*(-|or)\s*")]
    [name (partition 2 (map #(Long/parseLong %) rs))]))

(defn parse-ticket [s]
  (let [ns (str/split s #",")]
    (mapv #(Long/parseLong %) ns)))

(defn parse-input [s]
  (let [[rules my-t n-ts] (map str/split-lines (str/split s #"\n\n"))]
    [(map parse-rule rules)
     (parse-ticket (second my-t))
     (map parse-ticket (next n-ts))]))

(defn match-ticket [rs t]
  (reduce (fn [rs t] (if (some (fn [[_ ns]]
                                 (some (fn [[a b]] (<= a t b))
                                       ns))
                               rs)
                       rs
                       (reduced t)))
          rs t))

(defn part-1 [s]
  (let [[rules _ n-ts] (parse-input s)]
    (->> n-ts (map #(match-ticket rules %)) (filter number?) (reduce +))))

(comment
  (part-1 "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12")

  (part-1 input))
;26053

(defn match-ticket-2 [[rs m] t]
  [rs (reduce (fn [m [i t]]
                (reduce (fn [m [name ns]]
                          (if-not (some (fn [[a b]] (<= a t b)) ns)
                            (update m name (fnil conj #{}) i)
                            m))
                        m rs))
              m (map vector (range) t))])

(defn part-2 [s]
  (let [[rules my-t n-ts] (parse-input s)
        [_ failed] (->> n-ts
                        (remove #(number? (match-ticket rules %)))
                        (reduce match-ticket-2 [rules {}]))
        [r->i] (->> failed
                    (sort-by (comp - count second))
                    (reduce (fn [[rs available used] [name is]] (let [mine (set/difference available (set/difference is used))]
                                                                  [(conj rs [name (first mine)])
                                                                   (set/difference available mine)
                                                                   (set/union used mine)]))
                            [[] (into #{} (range (count rules))) #{}]))]
    (->> r->i
         (filter #(str/starts-with? (first %) "departure"))
         (map #(get my-t (second %)))
         (reduce *))))

(comment
  (part-2 "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12")

  (part-2 input))
;1515506256421
