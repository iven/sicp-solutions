#lang racket

(define (count-leaves l)
    (cond ((null? l) 0)
          ((not (pair? l)) 1)
          (else (+ (count-leaves (car l))
                   (count-leaves (cdr l))))))

(define (reverse l)
    (define (iter items result)
      (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
    (iter l null))

(define (deep-reverse l)
    (define (iter items result)
      (cond ((null? items) result)
            ((not (pair? items)) items)
            (else (iter (cdr items)
                        (cons (iter (car items) null) result)))))
    (iter l null))

(define (fringe l)
    (define (iter items result)
      (cond ((null? items) result)
            ((not (pair? items)) (cons items result))
            (else (iter (car items) (iter (cdr items) result)))))
    (iter l null))

(define test1 (cons (list 1 2) (list 3 4)))
(define test2 (list test1 test1))
(count-leaves test1)
(count-leaves test2)
(deep-reverse test1)
(fringe test1)
(fringe test2)
