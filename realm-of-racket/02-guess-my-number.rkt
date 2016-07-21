(define lower 1)
(define upper 100)

(define (guess)
  (quotient (+ lower upper) 2))

(define (smaller)
  (set! upper (max lower (sub1 (guess))))
  (guess))

(define (bigger)
  (set! lower (min upper (add1 (guess))))
  (guess))

(define (correct)
  (display "Yay!"))

(define (start n m)
  (set! lower (min n m))
  (set! upper (max n m))
  (guess))
