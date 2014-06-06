#lang racket

(require "accumulate.rkt")

(define (horner-eval x coefficient-sequence)
    (accumulate
      (lambda (a b) (+ (* x b) a))
      0
      coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
