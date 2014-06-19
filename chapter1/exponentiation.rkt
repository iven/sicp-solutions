#lang racket

(require "square.rkt")
(provide expt)

(define (expt b n)
    (cond	((= n 0) 1)
          ((= n 1) b)
          ((even? n) (square (expt b (/ n 2))))
          (else (* b (expt b (- n 1))))))

(module+
  main
  (expt 2 5)
  (expt 2 6))
