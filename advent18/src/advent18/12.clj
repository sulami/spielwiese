(ns advent18.12
  (:require [clojure.string :as str]))

(def input
  (->> "resources/12.input"
       slurp
       str/split-lines))

(def initial-state
  (->> input
       first
       (drop 15)
       (map #(= % \#))))

(def mapping
  (->> input
       (drop 2)
       (map #(re-find #"([#.]{5}) => (#|.)" %))
       (map rest)
       flatten
       (map (partial map #(= % \#)))
       (map #(if (= 1 (count %)) (first %) %))
       (apply hash-map)))

(defn generation [prev]
  (let [expanded-prev (concat (repeat 4 false) prev (repeat 4 false))]
    (for [i (range (- (count expanded-prev) 5))]
      (->> expanded-prev
           (drop i)
           (take 5)
           mapping))))

(def part-one
  (->> (nth (iterate generation initial-state) 20)
       (map #(vector %1 %2) (range (- (* 2 20)) 1000))
       (filter second)
       (map first)
       (reduce +)))

(defn -main []
  (println part-one))
