(ns day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (str/split s #"\n\n"))

(defn q-set [ss] (set (filter #(<= (int \a) (int %) (int \z)) ss)))

(defn part-1 [s]
  (->> (parse-input s)
       (map q-set)
       (map count)
       (reduce +)))

(comment
  (part-1 "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")

  (part-1 input))
;6351

(defn all-yes [s]
  (->> (str/split-lines s)
       (map q-set)
       (apply set/intersection)))

(defn part-2 [s]
  (->> (parse-input s)
       (map all-yes)
       (map count)
       (reduce +)))

(comment
  (part-2 "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")

  (part-2 input))
;3143
