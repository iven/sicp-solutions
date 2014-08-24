#lang racket

(define (count-pairs-old x)
    (if (not (pair? x))
      0
      (+ (count-pairs-old (car x))
         (count-pairs-old (cdr x))
         1)))

(define (count-pairs x)
    (let ((counted null))
      (define (iter item)
        (if (or (not (pair? item))
                (memq item counted))
          0
          (begin
            (set! counted (cons item counted))
            (+ 1
               (iter (car item))
               (iter (cdr item))))))
      (iter x)))

(module+
  main
  (define a1 (cons 1 1))
  (define b1 (cons 1 1))
  (define c1 (cons a1 b1))
  (count-pairs-old c1)
  (count-pairs c1)

  (define a2 (cons 1 1))
  (define b2 (cons a2 a2))
  (define c2 (cons 1 b2))
  (count-pairs-old c2)
  (count-pairs c2)

  (define a3 (cons 1 1))
  (define b3 (cons a3 a3))
  (define c3 (cons b3 b3))
  (count-pairs-old c3)
  (count-pairs c3))
