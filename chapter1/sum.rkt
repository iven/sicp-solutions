#lang racket

(provide cube sum inc)

(define (cube a) (* a a a))

(define (inc n) (+ n 1))

(define (sum-cubes a b) (sum cube a inc b))

(define (sum-origin term a next b)
    (if (> a b)
      0
      (+ (term a)
         (sum-origin term (next a) next b))))

(define (sum term a next b)
    (define (iter a result)
      (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
    (iter a 0))
