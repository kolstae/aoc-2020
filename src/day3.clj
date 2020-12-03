(ns day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (str/split-lines s))

(defn slope-count [lines dx dy]
  (let [width (count (first lines))
        height (count lines)]
    (->> (map (fn [x y] (nth (nth lines y) x))
              (map #(mod % width) (range 0 (* 100000 dx) dx))
              (range 0 height dy))
         (filter #{\#})
         count)))

(defn part-1 [s]
  (slope-count (parse-input s) 3 1))

(comment
  (part-1 "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#")

  (part-1 input))
;223

(defn part-2 [s]
  (let [lines (parse-input s)]
    (reduce * (map (fn [dx dy] (slope-count lines dx dy))
                   [1 3 5 7 1]
                   [1 1 1 1 2]))))

(comment
  (part-2 "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#")

  (part-2 input))
;3517401300
