#lang racket

(require "square.rkt")
(provide prime?)

(define (search-for-primes a b)
  (if (> a b)
    (void)
    (begin (timed-prime-test a) (search-for-primes (+ a 1) b))))

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n (- (current-inexact-milliseconds) start-time))
    (void)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(module+
  main
  (prime? 199)
  (prime? 1999)
  (prime? 19999))
