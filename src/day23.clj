(ns day23
  (:require [clojure.string :as str]))

(def input "538914762")

(defn parse-input [s]
  (map #(- (int %) (int \0)) s))

(defn ptr->seq [c ptr] (take (count ptr) (iterate #(get ptr (dec %)) c)))

(defn do-turn [n]
  (fn [[c ptr]]
    (let [f-p (get ptr (dec c))
          m-p (get ptr (dec f-p))
          l-p (get ptr (dec m-p))
          dest (inc (first (remove (comp #{f-p m-p l-p} inc) (next (iterate #(mod (dec %) n) (dec c))))))
          n-ptr (-> ptr
                    (assoc (dec c) (get ptr (dec l-p)))
                    (assoc (dec dest) f-p)
                    (assoc (dec l-p) (get ptr (dec dest))))]
      [(get n-ptr (dec c)) n-ptr])))

(defn do-turns [start ptr n]
  (->> [start ptr]
       (iterate (do-turn (count ptr)))
       (drop n)
       first
       second))

(defn ns->next-ptr [is]
  (->> is
       cycle
       (partition 2 1)
       (take (count is))
       (sort-by first)
       (mapv second)))

(defn part-1 [s]
  (let [is (parse-input s)]
    (->> (do-turns (first is) (ns->next-ptr is) 100)
         (ptr->seq 1)
         next
         str/join)))

(comment
  (part-1 "389125467")

  (part-1 input))
;"54327968"

(defn part-2 [s n]
  (let [is (concat (parse-input s) (range 10 1000001))]
    (->> (do-turns (first is) (ns->next-ptr is) n)
         (ptr->seq 1)
         next
         (take 2)
         (apply *))))

(comment
  (time (part-2 "389125467" 10000000))

  (part-2 input 10000000))
;157410423276
