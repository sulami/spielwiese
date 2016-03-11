; Factorial (n!).
(define (fact n)
  (if (eq? 1 n)
    n
    (* n (fact (- n 1)))))

;; Python-like range
(define range
  (case-lambda
    ((t) (range 0 t 1))
    ((f t) (range f t 1))
    ((f t s) (if (>= f t)
                 '()
                 (cons f (range (+ f s) t s))))))
