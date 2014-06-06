#lang racket

(define (cons x y)
    (define (dispatch m)
      (cond	((= m 0) x)
            ((= m 1) y)
            (else (error "Arguments not 0 or 1 -- CONS" m))))
    dispatch)

(define (car x) (x 0))
(define (cdr x) (x 1))

(define (print-cons x)
    (newline)
    (display "(")
    (display (car x))
    (display ".")
    (display (cdr x))
    (display ")"))

(module+ main
  (print-cons (cons 10 100)))
