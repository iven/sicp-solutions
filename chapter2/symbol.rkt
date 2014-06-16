#lang racket

(define (equal? a b)
    (cond ((and (pair? a) (pair? b))
           (and (equal? (car a) (car b))
                (equal? (cdr a) (cdr b))))
          ((and (not (pair? a)) (not (pair? b)))
           (eq? a b))
          (else #f)))

(module+
  main
  (equal? (list 'a 'b) (list 'a 'b))
  (equal? (list 'a 'b) (list 'c 'b))
  (equal? (list 'a (list 'b)) (list 'a (list 'b)))
  (equal? (list 'a (list 'b)) (list 'a (list 'c))))
