(ns advent18.02
  (:require [clojure.string :as str]))

(def input
  (->> "resources/02.input"
       slurp
       str/split-lines))

(defn count-occurences
  [x coll]
  (count (filter #{x} coll)))

(defn single-checksum
  [id]
  (let [occurences (map #(count-occurences % id) id)]
    [(some? (some #{2} occurences))
     (some? (some #{3} occurences))]))

(defn composite-checksum
  [counts]
  (let [twos (map first counts)
        threes (map second counts)]
    [(count (filter identity twos))
     (count (filter identity threes))]))

(def part-one
  (->> input
       (map single-checksum)
       composite-checksum
       (reduce *)))

(defn similar-ids?
  [a b]
  (->> (map = a b)
       (filter (complement identity))
       count
       (= 1)))

(defn has-sibling?
  [id ids]
  (some true? (map (partial similar-ids? id)
                   (filter #(not= id %) ids))))

(defn remove-differences
  [a b]
  (->> (map #(if (= %1 %2) %1) a b)
       (filter some?)
       (apply str)))

(def part-two
  (->> input
       (map #(-> % (has-sibling? input) (vector %)))
       (filter #(-> % first true?))
       (map second)
       (apply remove-differences)))

(defn -main []
  (println part-one)
  (println part-two))
