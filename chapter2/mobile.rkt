#lang racket

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))
(define (left-branch x) (car x))
(define (right-branch x) (car (cdr x)))
(define (branch-length x) (car x))
(define (branch-structure x) (car (cdr x)))
(define (branch-weight x) (total-weight (branch-structure x)))
(define (branch-torque x) (* (branch-length x) (branch-weight x)))

(define (total-weight x)
    (if (pair? x)
      (+ (total-weight (branch-structure (left-branch x)))
        (total-weight (branch-structure (right-branch x))))
      x))

(define (balance? x)
    (if (pair? x)
      (let ((l (left-branch x))
            (r (right-branch x)))
        (and (balance? (branch-structure l))
             (balance? (branch-structure r))
             (= (branch-torque l) (branch-torque r))))
    true))

(define test1 (make-mobile (make-branch 5
                                        (make-mobile (make-branch 3 4)
                                                     (make-branch 2 6)))
                           (make-branch 2 25)))
(total-weight test1)
(branch-torque (left-branch test1))
(balance? test1)
