; Factorial (n!).
(define (fact n)
         (if (eq? 1 n)
            n
            (* n (fact (- n 1)))))

; Construct a list from s to e defining the step size.
(define (cls s ss e)
         (if (> s e)
            '()
            (cons s (cls (+ s ss) ss e))))

; Construct a list from s to e.
(define (cl s e)
        (cls s 1 e))

; Construct a list from 1 to e with step size 1.
(define (oneto e)
        (cl 1 e))

