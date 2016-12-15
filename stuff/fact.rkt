#lang typed/racket

(: fact (-> Integer Integer))
(define (fact n)
  (foldl * 1 (cdr (range (+ n 1)))))
