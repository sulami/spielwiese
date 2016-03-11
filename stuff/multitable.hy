(defn table [a b]
  (for (i (range a))
    (for (j (range b))
      (let ((ij (* (+ 1 i) (+ 1 j)) ))
        (apply print [(.format "{:4}" ij)]
                     {"end" ""})))
    (print)))
