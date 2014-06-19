#lang racket

(require "square.rkt")

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define dx 0.00001)

(define (smooth f)
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx)) 3))))

(module+ main
  ((smooth square) 5)
  ((repeated square 3) 2))
