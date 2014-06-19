#lang racket

(provide square)

(define (square x) (* x x))

(module+
  main
  (square 4))

