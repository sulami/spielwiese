(ns golden-rose
  "This is a quickly hacked together implementation of the Golden Rose Kata.")

(defn age [item base]
  (let [overaged (-> item :sell-in neg?)]
    (-> item
        :quality
        (- (if overaged
             (* base 2)
             base))
        (min 50)
        (max 0))))

(defn degrade-normal [item]
  (age item 1))

(defn degrade-conjured [item]
  (age item 2))

(defn degrade-brie [item]
  (age item -1))

(defn degrade-sulfuras [item]
  80)

(defn degrade-backstage-passes [item]
  (-> (cond
        (-> item :sell-in neg?) 0
        (-> item :sell-in (< 5)) (-> item :quality (+ 3))
        (-> item :sell-in (< 10)) (-> item :quality (+ 2))
        :else (-> item :quality inc))
      (min 50)
      (max 0)))

(defn degrade [item]
  (case (:type item)
    :brie (degrade-brie item)
    :sulfuras (degrade-sulfuras item)
    :backstage-passes (degrade-backstage-passes item)
    :conjured (degrade-conjured item)
    (degrade-normal item)))

(defn tick [item]
  (-> item
      (update-in [:quality] (constantly (degrade item)))
      (update-in [:sell-in] dec)))
