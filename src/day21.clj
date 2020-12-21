(ns day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-ingredients [s]
  (map #(re-seq #"\w+" %) (str/split s #"\s+\(contains\s+")))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map parse-ingredients)))

(defn part-1 [s]
  (let [is (parse-input s)
        a->is (reduce (fn [m [is as]]
                        (reduce (fn [m a] (apply update m a conj is)) m as))
                      {} is)]
    (let [as (->> a->is
                  (map (comp frequencies val))
                  (mapcat (fn [fs] (let [m (apply max (vals fs))]
                                     (map first (filter (comp #{m} val) fs)))))
                  set)]
      (prn as)
      (reduce + (vals (apply dissoc (frequencies (mapcat first is)) as))))))

(comment
  (part-1 "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)")

  (part-1 input))
;2556

(defn w-d-max [a->fs]
  (first (keep (fn [[a fs]] (let [m (apply max (map second fs))
                                  w-max (filter (comp #{m} second) fs)]
                              (when (= 1 (count w-max))
                                [(ffirst w-max) a])))
               a->fs)))

(defn part-2 [s]
  (let [is (parse-input s)
        a->is (reduce (fn [m [is as]]
                        (reduce (fn [m a] (apply update m a conj is)) m as))
                      {} is)]
    (let [fs (into {} (map (juxt key (comp frequencies val)) a->is))]
      (loop [as {} fs fs]
        (if (seq fs)
          (let [[i a] (w-d-max fs)]
            (recur (assoc as a i) (into {} (map (juxt key (comp #(dissoc % i) val)) (dissoc fs a)))))
          (str/join \, (map second (sort-by first (seq as)))))))))

(comment
  (part-2 "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)")

  (part-2 input))
;"vcckp,hjz,nhvprqb,jhtfzk,mgkhhc,qbgbmc,bzcrknb,zmh"
