#lang racket

(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

(define (display-interval i)
    (newline)
    (display "[")
    (display (lower-bound i))
    (display ",")
    (display (upper-bound i))
    (display "]"))

(define (add-interval x y)
    (make-interval (+ (upper-bound x) (upper-bound y))
                   (+ (lower-bound x) (lower-bound y))))

(define (sub-interval x y)
    (make-interval (- (upper-bound x) (lower-bound y))
                   (- (lower-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (upper-bound x) (upper-bound y)))
          (p2 (* (upper-bound x) (lower-bound y)))
          (p3 (* (lower-bound x) (upper-bound y)))
          (p4 (* (lower-bound x) (upper-bound y))))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval x
                  (make-interval (/ (1.0 (upper-bound y)))
                                 (/ (1.0 (lower-bound y))))))

(define (make-center-percent c p)
    (let ((width (* c (/ p 100.0))))
      (make-interval (+ c width) (- c width))))

(define (percent x)
    (let ((center (/ (+ (upper-bound x) (lower-bound x)) 2.0))
          (width (/ (- (upper-bound x) (lower-bound x)) 2.0)))
      (* (/ width center) 100.0)))

(module+ main
  (define i (make-interval 2 7))
  (define j (make-interval 8 3))
  (define k (make-center-percent 100 20))

  (display-interval i)
  (display-interval j)
  (display-interval k)
  (display-interval (sub-interval i j))
  (display-interval (sub-interval j i)))
