(ns day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-instr [s]
  (str/split s #"\[|\]?\s*=\s*"))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map parse-instr)))

(def zero-bits (str/join (repeat 36 \0)))

(defn apply-mask [mask n]
  (reduce (fn [s [i c]]
            (str (subs s 0 i) c (subs s (inc i))))
          (str (subs zero-bits (count n)) n)
          mask))

(defn apply-mask-l [mask n]
  (Long/parseLong (apply-mask mask (Long/toBinaryString (Long/parseLong n))) 2))

(defn do-instr [state [op a b]]
  (case op
    "mask" (assoc state :mask (into {} (comp (map-indexed vector)
                                             (filter (comp #{\0 \1} second))) a))
    "mem" (assoc-in state [:mem (Long/parseLong a)] (apply-mask-l (:mask state) b))
    state))

(defn part-1 [s]
  (reduce + (vals (:mem (reduce do-instr {:mem {}} (parse-input s))))))

(comment
  (part-1 "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0")

  (part-1 input))
;14839536808842

(defn all-subsets [s]
  (loop [[f & r] (seq s) p '(#{})]
    (if f
      (recur r (concat p (map #(conj % f) p)))
      p)))

(defn apply-mask-xs [mask xs s]
  (let [n (apply-mask mask (Long/toBinaryString (Long/parseLong s)))
        cs (vec n)]
    (->> xs
         (map (fn [xs] (->> xs (reduce #(update %1 %2 {\0 \1 \1 \0}) cs) str/join)))
         (map #(Long/parseLong % 2)))))

(defn do-instr2 [state [op a b]]
  (case op
    "mask" (-> state
               (merge (into {} (map (fn [[k v]] [k (into {} v)])) (group-by #(get {\X :xs \1 :mask} (second %) :discard) (map-indexed vector a))))
               (dissoc :discard)
               (update :xs (comp all-subsets keys)))
    "mem" (let [v (Long/parseLong b)]
            (assoc state :mem (reduce (fn [mem a] (assoc mem a v)) (:mem state) (apply-mask-xs (:mask state) (:xs state) a))))
    state))

(defn part-2 [s]
  (reduce + (vals (:mem (reduce do-instr2 {:mem {}} (parse-input s))))))

(comment
  (part-2 "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1")

  (part-2 input))
;4215284199669
