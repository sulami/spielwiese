; Factorial (n!).
(define fact
        (lambda (n)
          (if (eq? 1 n)
            n
            (* n (fact (- n 1))))))

; Construct a list from s to e.
(define cl
        (lambda (s e)
          (if (> s e)
            '()
            (cons s (cl (+ s 1) e)))))

