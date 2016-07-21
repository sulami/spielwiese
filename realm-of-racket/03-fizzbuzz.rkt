#lang racket

(define (check n)
  (cond [(= 0 (remainder n 15)) "FizzBuzz"]
        [(= 0 (remainder n 3)) "Fizz"]
        [(= 0 (remainder n 5)) "Buzz"]
        [else n]))

(define (range n)
  (define (in-range m l)
    (if (= 1 m)
      (cons m l)
      (in-range (sub1 m) (cons m l))))
  (in-range n empty))

(define (main)
  (map check (range 100)))

(main)
