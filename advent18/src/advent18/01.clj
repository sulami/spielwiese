(ns advent18.01
  (:require [clojure.string :as str]))

(def input
  (->> "resources/01.input"
       slurp
       str/split-lines
       (map read-string)))

(def part-one
  (reduce + input))

(defn add-and-compare
  [[seen curr] coll]
  (let [next-number (-> coll first (+ curr))]
    (if (seen next-number)
      next-number
      (recur [(conj seen next-number) next-number] (rest coll)))))

(def part-two
  (add-and-compare [#{} 0] (cycle input)))

(defn -main []
  (println part-one)
  (println part-two))
