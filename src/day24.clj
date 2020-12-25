(ns day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (->> s (str/split-lines)
       (map (fn [s] (map keyword (re-seq #"e|w|ne|nw|se|sw" s))))))

(def dir {:e [-1 1 0] :w [1 -1 0]
          :ne [0 1 -1] :sw [0 -1 1]
          :se [-1 0 1] :nw [1 0 -1]})

(defn black-tiles [s]
  (->> (parse-input s)
       (map (fn [ds] (reduce (fn [p d] (mapv + p (dir d))) [0 0 0] ds)))
       frequencies
       (filter (comp odd? val))
       keys))

(defn part-1 [s]
  (count (black-tiles s)))

(comment
  (part-1 "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew")

  (part-1 input))
;495

(defn surrounding [t]
  (map #(mapv + t %) (vals dir)))

(defn flip [tiles]
  (let [t->ns (map (juxt identity surrounding) tiles)
        ts (->> t->ns
                (remove (fn [[_ ns]] (let [cnt (count (filter tiles ns))] (or (zero? cnt) (< 2 cnt)))))
                (map first)
                set)
        nfs (reduce (fn [fs [_ ns]]
                      (reduce #(update %1 %2 (fnil inc 0)) fs (remove tiles ns)))
                    {} t->ns)]
    (into ts
          (comp (filter (comp #(= 2 %) val))
                (map key))
          nfs)))

(defn part-2 [s]
  (->> (black-tiles s)
       set
       (iterate flip)
       (drop 100)
       (map count)
       first))

(comment
  (part-2 "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew")

  (part-2 input))
;4012
