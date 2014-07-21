#lang racket

(provide (rename-out [element-of-set? element-of-set-unsorted-list?]))

(define (element-of-set? e s)
    (if (null? s)
      false
      (or (equal? e (car s))
          (element-of-set? e (cdr s)))))

(define (adjoin-set e s)
    (if (element-of-set? e s)
      s
      (cons e s)))

(define (intersection-set s1 s2)
    (cond	((or (null? s1) (null? s2)) null)
          ((element-of-set? (car s1) s2)
           (adjoin-set (car s1) (intersection-set (cdr s1) s2)))
          (else (intersection-set (cdr s1) s2))))

(define (union-set s1 s2)
    (cond	((null? s1) s2)
          ((null? s2) s1)
          ((element-of-set? (car s1) s2) (union-set (cdr s1) s2))
          (else (adjoin-set (car s1) (union-set (cdr s1) s2)))))


(module+
  main
  (define test1 (list 1 2 3))
  (define test2 (list 1 3 5))
  (element-of-set? 2 test1)
  (element-of-set? 2 test2)
  (adjoin-set 2 test1)
  (adjoin-set 4 test2)
  (intersection-set test1 test2)
  (union-set test1 test2))
