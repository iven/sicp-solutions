#lang racket

(provide accumulate accumulate-n)

(define (accumulate op initial sequence)
    (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial sequence)
    (if (null? (car sequence))
      null
      (cons (accumulate
              op
              initial
              (map car sequence))
            (accumulate-n op initial (map cdr sequence)))))

(define squares (list 0 1 4 9 16 25))
(define test1 (list (list 1 2 3)
                    (list 4 5 6)
                    (list 7 8 9)
                    (list 10 11 12)))

(accumulate
  (lambda (x y) (+ x y))
  0
  squares)
(accumulate-n + 0 test1)
