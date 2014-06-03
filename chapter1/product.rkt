#lang racket

(define (product-origin term a next b)
    (if (> a b)
      1.0
      (* (term a)
         (product-origin term (next a) next b))))

(define (product term a next b)
    (define (iter a result)
      (if (> a b)
        result
        (iter (next a) (* (term a) result))))
    (iter a 1.0))

(define (inc n) (+ n 1))

(define (term-pi n)
    (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))

(* (product term-pi 1 inc 1000) 4)
(* (product term-pi 1 inc 10000) 4)
(* (product term-pi 1 inc 100000) 4)
(* (product term-pi 1 inc 1000000) 4)
(* (product-origin term-pi 1 inc 1000000) 4)
