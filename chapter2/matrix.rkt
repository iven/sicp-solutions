#lang racket

(require "accumulate.rkt")

(define matrix (list (list 1 2 3 4)
                     (list 5 6 7 8)
                     (list 9 10 11 12)))

(define (dot-product v1 v2) (accumulate + 0 (map * v1 v2)))

(define (matrix-*-vector m v)
    (map (lambda (row) (dot-product row v))
         m))

(define (transpose m)
    (accumulate-n cons null m))

(define (matrix-*-matrix m1 m2)
    (let ((cols (transpose m2)))
      (map (lambda (row) (matrix-*-vector cols row))
           m1)))

(module+ main
  (matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2))))
