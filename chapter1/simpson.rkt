#lang racket

(require "sum.rkt")

(define (round-to-next-even x)
  (+ x (remainder x 2)))

(define (simpson f a b n)
  (define fixed-n (round-to-next-even n))
  (define h (/ (- b a) fixed-n))
  (define (simpson-term k)
    (define y (f (+ a (* k h))))
    (if (or (= k 0) (= k fixed-n))
      y
      (if (even? k)
        (* 2 y)
        (* 4 y))))
  (* (/ h 3) (sum simpson-term 0 inc fixed-n)))

(module+ main
  (simpson cube 0 1 100)
  (simpson cube 0 1 1000))
