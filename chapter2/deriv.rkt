#lang racket

(define (varible? e) (symbol? e))
(define (same-varible? x y) (and (varible? x) (varible? y) (eq? x y)))
(define (=number? e num) (and (number? e) (= e num)))

(define (sum? e) (and (pair? e) (eq? (car e) '+)))
(define (addend e) (cadr e))
(define (augend e) (caddr e))
(define (make-sum x y)
    (cond	((=number? x 0) y)
          ((=number? y 0) x)
          ((and (number? x) (number? y)) (+ x y))
          (else (list '+ x y))))

(define (product? e) (and (pair? e) (eq? (car e) '*)))
(define (multiplier e) (cadr e))
(define (multiplicand e) (caddr e))
(define (make-product x y)
    (cond	((=number? x 1) y)
          ((=number? y 1) x)
          ((or (=number? x 0) (=number? y 0)) 0)
          ((and (number? x) (number? y)) (* x y))
          (else (list '* x y))))

(define (deriv exp var)
    (cond	((number? exp) 0)
          ((varible? exp) (if (same-varible? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum (make-product (multiplier exp)
                                   (deriv (multiplicand exp) var))
                     (make-product (multiplicand exp)
                                   (deriv (multiplier exp) var))))
          (else (error "Unknown expression type -- DERIV" exp))))

(module+
  main
  (deriv '(+ x 3) 'x)
  (deriv '(* x y) 'x)
  (deriv '(* (* x y) (+ x 3)) 'x))
