#lang racket

(require "accumulate.rkt")
(require "enumerate.rkt")

(define (sum-equals? l s)
    (= s
       (+ (car l)
          (cadr l)
          (cadr (cdr l)))))

(define (unique-pairs n)
    (flat-map (lambda (x)
                (flat-map (lambda (y)
                            (map (lambda (z) (list x y z))
                                 (enumerate-interval 1 (- y 1))))
                          (enumerate-interval 1 (- x 1))))
              (enumerate-interval 1 n)))

(define (ordered-triples-sum n s)
    (filter (lambda (l) (sum-equals? l s))
            (unique-pairs n)))

(module+ main
  (define test1 (list 1 2 3))
  (sum-equals? test1 6)
  (sum-equals? test1 5)
  (ordered-triples-sum 9 9))
