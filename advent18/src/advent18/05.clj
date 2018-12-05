(ns advent18.05
  (:require [clojure.string :as str]))

(def input
  (->>  "../../resources/05.input"
        slurp
        str/trim))

(defn f [acc coll]
  (if (empty? coll)
    acc
    (let [head (first coll)
          tail (rest coll)]
      (if (empty? tail)
        (conj acc head)
        (if (and (= (str/upper-case head) (str/upper-case (first tail)))
                 (not= head (first tail)))
          (recur acc (rest tail))
          (recur (conj acc head) tail))))))

(defn iterate-until-unchanged [f x]
  (->> x
       (iterate f)
       (reduce #(if (= %1 %2) (reduced %1) %2))))

(def part-one
  (->> input
       (iterate-until-unchanged (partial f []))
       count))

(def part-two
  (->> input
       str/lower-case
       set
       (map (fn [c] (->> input
                         (filter #(not= (str/upper-case %) (str/upper-case c)))
                         (iterate-until-unchanged (partial f []))
                         count)))
       (reduce min)))

(defn -main []
  (println part-one)
  (println part-two))
