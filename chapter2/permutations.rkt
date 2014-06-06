#lang racket

(require "accumulate.rkt")

(define (permutations s)
    (if (null? s)
      (list null)
      (flat-map (lambda (x)
                  (map (lambda (y) (cons x y))
                       (permutations (remove x s))))
                s)))

(module+ main
  (define test1 (list 1 2 3))
  (permutations test1))
