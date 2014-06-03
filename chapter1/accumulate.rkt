#lang racket

(define (accumulate-origin combiner null-value term a next b)
    (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-origin combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
      (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
    (iter a null-value))

(define (product-origin term a next b)
    (accumulate-origin * 1.0 term a next b))

(define (product term a next b)
    (accumulate * 1.0 term a next b))

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

