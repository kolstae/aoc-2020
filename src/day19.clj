(ns day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (->> (str/split s #"\n\n")
       (map str/split-lines)))

(defn parse-rules [rs]
  (into {} (map (fn [r] (let [[n r] (str/split r #":\s*")]
                          [n (map #(str/split % #"\s+") (str/split r #"\s*[|]\s*"))]))
                rs)))

(defn make-rule [m rs]
  (let [es (map (fn [xs] (str/join (map #(if (= (first %) \") (subs % 1 (dec (count %))) (make-rule m (get m %)))
                                        xs)))
                rs)]
    (if (next es)
      (str \( (str/join \| es) \))
      (first es))))

(defn part-1 [s]
  (let [[rs ms] (parse-input s)
        n->r (parse-rules rs)
        rule (re-pattern (make-rule n->r (n->r "0")))]
    (count (filter #(re-matches rule %) ms))))

(comment
  (part-1 "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb")

  (part-1 input))
;126

(defn iter-0 [r8 [r42 r31] max-n]
  (fn [s] (loop [n 1]
            (when (<= n max-n)
              (if (re-matches (re-pattern (str/join (cons r8 (concat (repeat n r42) (repeat n r31))))) s)
                true
                (recur (inc n)))))))

(defn part-2 [s]
  (let [[rs ms] (parse-input s)
        n->r (parse-rules rs)
        r31 (make-rule n->r (n->r "31"))
        r42 (make-rule n->r (n->r "42"))
        r8 (str r42 \+)]
    (count (filter (iter-0 r8 [r42 r31] 30) ms))))

(comment
  (part-2 "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")

  (part-2 input))
;282
