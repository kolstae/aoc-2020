(ns day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (vec (map vec (str/split-lines s))))

(defn surrounding-state [state x y]
  (->> (for [nx (range (dec x) (inc (inc x)))
             ny (range (dec y) (inc (inc y)))
             :when (and (nat-int? nx) (nat-int? ny) (not (and (= nx x) (= ny y))))]
         [ny nx])
       (keep #(get-in state %))))

(defn next-state [st f n state [x y]]
  (let [tile (get-in st [y x])]
    (cond-> state
      (and (= tile \L)
           (->> (f st x y)
                (every? #{\L \.})))
      (assoc-in [y x] \#)

      (and (= tile \#) (->> (f st x y)
                            (filter #{\#})
                            (count)
                            (<= n)))
      (assoc-in [y x] \L))))

(defn do-next [state f n]
  (let [mx (count (first state))
        my (count state)]
    (reduce #(next-state state f n %1 %2) state (for [x (range mx) y (range my)] [x y]))))

(defn part-1 [s]
  (let [state (parse-input s)]
    (->> (iterate #(do-next % surrounding-state 4) state)
         (partition 2 1)
         (drop-while #(apply not= %))
         (ffirst)
         (mapcat #(filter #{\#} %))
         count)))

(comment
  (part-1 "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")

  (part-1 input))
;2254

(defn seen-seats [state x y]
  (->> (map (fn [f ds]
              (->> ds
                   (keep #(->> (f %) (get-in state) #{\L \#}))
                   (first)))
            [#(vector (- y %) (- x %))
             #(vector (- y %) x)
             #(vector (- y %) (+ x %))
             #(vector y (- x %))
             #(vector (+ y %) (+ x %))
             #(vector (+ y %) x)
             #(vector (+ y %) (- x %))
             #(vector y (+ x %))]
            (repeat (range 1 (count state))))
       (filter identity)))

(defn part-2 [s]
  (->> (parse-input s)
       (iterate #(do-next % seen-seats 5))
       (partition 2 1)
           (drop-while #(apply not= %))
           (ffirst)
           (mapcat #(filter #{\#} %))
           count))

(comment
  (part-2 "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")

  (part-2 input))
;2004
