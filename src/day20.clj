(ns day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn make-num [cs]
  (reduce (fn [^long n c] (cond-> (bit-shift-left n 1)
                            (= c \#) (bit-or 1)))
          0 cs))

(defn d->img [d]
  {:data d
   :up (make-num (first d))
   :left (make-num (map first d))
   :right (make-num (map last d))
   :down (make-num (last d))})

(defn parse-img [s]
  (let [[t & d] (str/split-lines s)
        ts (re-seq #"\d+" t)]
    [(Long/parseLong (first ts))
     (d->img d)]))

(defn parse-input [s]
  (->> (str/split s #"\n\n")
       (map parse-img)))

(defn rotate-img [{:keys [data id]}]
  (assoc (d->img (apply map (fn [& cs] (str/join cs)) data))
    :id id))

(defn sides [{:keys [up left right down]}]
  [left down up right])

(defn variations [img]
  (distinct
    (concat
      (take 2 (iterate rotate-img img))
      (take 2 (next (iterate rotate-img (update img :data reverse))))
      (take 2 (next (iterate rotate-img (update img :data #(map reverse %)))))
      (take 2 (next (iterate rotate-img (update img :data #(map reverse (reverse %)))))))))

(defn rotations [[id img]]
  [id
   (into #{}
         (mapcat sides)
         (variations img))])

(defn part-1 [s]
  (let [sides (into {} (map rotations) (parse-input s))]
    (->> sides
         (map (fn [[id ss]]
                [id
                 (keep (fn [[oid oss]]
                         (when (and (not= id oid) (seq (set/intersection oss ss))) oid))
                       sides)]))
         (filter (comp #{2} count second))
         (map first)
         (reduce *))))

(comment
  (part-1 "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###...")

  (part-1 input))
;54755174472007

(defn both-neighbour [{:keys [right down] :as img} [i1 i2]]
  (first (for [i1 (variations i1)
               i2 (variations i2)
               :when (or (and (= right (:left i1)) (= down (:up i2)))
                         (and (= right (:left i2)) (= down (:up i1))))]
           (apply vector img (if (= right (:left i1)) [i1 i2] [i2 i1])))))

(defn neighbour [[k-f k-t] img is]
  (->> is
       (mapcat variations)
       (filter #(= (k-f img) (k-t %)))
       first))

(defn worm?
  ([ls] (worm? ls nil))
  ([[l1 l2 l3] n]
   (if-let [[m] (re-seq #"#....##....##....###" l2)]
     (let [i (str/index-of l2 m)
           [l1 l2 l3] (map #(subs % i) [l1 l2 l3])]
       (if (and (= \# (.charAt l1 18))
                (re-matches #"^.#..#..#..#..#..#.+" l3))
         (recur (map #(subs % 20) [l1 l2 l3]) (inc (or n 0)))
         (recur (map #(subs % 1) [l1 l2 l3]) n)))
     n)))

(defn part-2 [s]
  (let [tiles (into {} (map (fn [[id img]] [id (assoc img :id id)])) (parse-input s))
        sides (into {} (map rotations) tiles)
        id->matches (->> sides
                         (map (fn [[id ss]]
                                [id
                                 (->> sides
                                      (keep (fn [[oid oss]]
                                              (when (and (not= id oid) (seq (set/intersection oss ss)))
                                                oid)))
                                      (into #{}))]))
                         (into {}))
        [lu is] (->> id->matches
                     (filter (comp #{2} count second))
                     first
                     ((juxt (comp tiles first) (comp #(map tiles %) second))))
        [lu r] (first (keep #(both-neighbour % is) (variations lu)))
        ts (loop [r r
                  row 0
                  done #{(:id lu)}
                  ts [[lu]]]
             (let [done (conj done (:id r))
                   ts (update ts row (fnil conj []) r)
                   oids (set/difference (id->matches (:id r)) done)]
               (if-let [rn (neighbour [:right :left] r (map tiles oids))]
                 (recur rn row done ts)
                 (let [r (get-in ts [row 0])
                       oids (set/difference (id->matches (:id r)) done)]
                   (if-let [dn (neighbour [:down :up] r (map tiles oids))]
                     (recur dn (inc row) done ts)
                     ts)))))
        images (->> ts
                    (map (fn [ts] (map (comp #(map (comp next butlast) (butlast (next %))) :data) ts)))
                    (mapcat (fn [ts] (map (comp str/join flatten) (apply map vector ts))))
                    (assoc {} :data)
                    variations
                    (map :data))]
    (- (count (filter #{\#} (mapcat identity (first images))))
       (* 15 (reduce + (mapcat #(keep worm? (partition 3 1 %)) images))))))

(comment
  (part-2 "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###...")

  (part-2 input))
;1692
