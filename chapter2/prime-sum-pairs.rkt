#lang racket

(require "../chapter1/prime.rkt")
(require "accumulate.rkt")
(require "enumerate.rkt")

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum?
                 (flat-map (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(enumerate-interval 1 6)
(prime-sum-pairs 6)
