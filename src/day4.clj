(ns day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (-> (.getName *ns*) (str ".txt") io/resource slurp))

(defn parse-input [s]
  (->> (str/split s #"\n\n")
       (map #(->> (str/split % #"[\s:]+")
                  (partition 2)))))

(defn valid-passport [passport]
  (set/subset? #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"} (set (map first passport))))

(defn part-1 [s]
  (count (filter valid-passport (parse-input s))))

(comment
  (part-1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")

  (part-1 input))
;213

(defn valid-passport-2 [passport]
  (and (valid-passport passport)
       (every? (fn [[k v]]
                 (let [res (case k
                             "byr" (<= 1920 (Integer/parseInt v) 2002)
                             "iyr" (<= 2010 (Integer/parseInt v) 2020)
                             "eyr" (<= 2020 (Integer/parseInt v) 2030)
                             "hgt" (when-let [[_ l unit] (re-matches #"^(\d+)(in|cm)$" v)]
                                     (case unit
                                       "cm" (<= 150 (Integer/parseInt l) 193)
                                       "in" (<= 59 (Integer/parseInt l) 76)
                                       false))
                             "hcl" (re-matches #"^#[a-f0-9]{6}$" v)
                             "ecl" (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v)
                             "pid" (re-matches #"^\d{9}$" v)
                             "cid" true
                             false)]
                   ;(prn k v res)
                   res))
               passport)))

(defn part-2 [s]
  (count (filter valid-passport-2 (parse-input s))))

(comment
  (part-2 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")
  (part-2 "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\niyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946\n\nhcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\nhgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007")
  (part-2 "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f\n\neyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\nhcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022\n\niyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

  (part-2 input))
;147
