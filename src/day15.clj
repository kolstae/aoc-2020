(ns day15)

(defn n-game [ns x]
  (let [m (zipmap ns (map inc (range)))]
    (->> [(last ns) (count m) (dissoc m (last ns))]
         (iterate (fn [[pn i m]]
                    (let [n (if-let [l (get m pn)]
                              (- i l)
                              0)]
                      [n (inc i) (assoc m pn i)])))
         (drop (- x (count ns)))
         ffirst)))

(defn part-1 [ns]
  (n-game ns 2020))

(comment
  (part-1 [0 3 6])
  (part-1 [1 3 2])
  (part-1 [2,1,3])
  (part-1 [1,2,3])
  (part-1 [2,3,1])
  (part-1 [3,1,2])

  (part-1 [8 13 1 0 18 9]))
;755

(defn part-2 [ns]
  (n-game ns 30000000))

(comment
  (part-2 [0 3 6])
  (part-2 [1 3 2])
  (part-2 [2,1,3])
  (part-2 [1,2,3])
  (part-2 [2,3,1])
  (part-2 [3,1,2])

  (part-2 [8 13 1 0 18 9]))
;11962
