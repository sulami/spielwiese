(defn table [a b]
  (for (i (list (range a)))
    (for (j (list (range b)))
      (let ((ij (* (+ 1 i) (+ 1 j)) ))
        (apply print [(.format "{:4}" ij)]
                     {"end" ""})))
    (print)))
