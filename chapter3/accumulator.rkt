#lang racket

(define (make-accumulator sum)
    (lambda (x)
      (begin (set! sum (+ sum x))
             sum)))

(module+
  main
  (define A (make-accumulator 5))
  (A 10)
  (A 10))
