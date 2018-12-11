(ns advent18.11)

(def input 8141)

(defn power-level [[x y]]
  (-> 10
      (+ x)
      (* y)
      (+ input)
      (* (+ x 10))
      str
      (->> (str "00000"))
      reverse
      (nth 2)
      str
      read-string
      (- 5)))

(defn square-level [[x y]]
  (reduce + (for [a (range x (+ x 3))
                  b (range y (+ y 3))]
              (power-level [a b]))))

(def part-one
  (->> (for [x (range 1 298)
             y (range 1 298)]
         [[x y] (square-level [x y])])
       (apply max-key second)
       first
       (map str)
       (interpose ",")
       (apply str)))

(defn -main []
  (println (part-one)))
