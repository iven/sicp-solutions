#lang racket

(define f
    (let ((init 0))
      (lambda (x)
        (if (= init 0)
          (begin (set! init 1) x)
          0))))

(module+
  main
  ;(+ (f 0) (f 1))
  (+ (f 1) (f 0)))
