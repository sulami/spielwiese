(ns advent18.03
  (:require [clojure.string :as str]))

(def input
  (-> "resources/03.input"
      slurp
      str/split-lines))

(defn read-claim [s]
  (->> (re-find #"(\d+),(\d+): (\d+)x(\d+)" s)
       rest
       (map read-string)))

(def claims (map read-claim input))

(def empty-grid
  (->> 0
       (repeat 1000)
       vec
       (repeat 1000)
       vec))

(defn inc-grid [grid claim]
  (let [[l t w h] claim
        coverage (for [x (range l (+ l w))
                       y (range t (+ t h))]
                   [x y])]
    (reduce (fn [g [x y]] (update-in g [x y] inc)) grid coverage)))

(def part-one
  (->> claims
       (reduce inc-grid empty-grid)
       (reduce concat)
       (filter #(> % 1))
       count))

(defn -main []
  (println part-one))
