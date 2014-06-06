#lang racket

(provide accumulate)

(define (list-ref l n)
    (if (= n 0)
      (car l)
      (list-ref (cdr l) (- n 1))))

(define (length-origin l)
    (if (null? l)
      0
      (+ 1 (length-origin (cdr l)))))

(define (length l)
    (define (length-iter l count)
      (if (null? l)
        count
        (length-iter (cdr l) (+ count 1))))
    (length-iter l 0))

(define (append l r)
    (if (null? l)
      r
      (cons (car l) (append (cdr l) r))))

(define (last-pair l)
    (if (null? (cdr l))
      (list (car l))
      (last-pair (cdr l))))

(define (reverse l)
    (define (iter items result)
      (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
    (iter l null))

(define (map proc items)
    (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (for-each proc items)
    (proc (car items))
    (if (null? (cdr items))
      true
      (for-each proc (cdr items))))

(define (filter proc items)
    (if (null? items)
      null
      (let ((first (car items))
            (rest (cdr items)))
        (if (proc first)
          (cons first (filter proc rest))
          (filter proc rest)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define squares (list 0 1 4 9 16 25))
(define odds (list 1 3 5 7 9))

(list-ref squares 3)
(length squares)
(append squares odds)
(last-pair squares)
(reverse squares)
(map (lambda (x) (* x 10)) odds)
(for-each
  (lambda (x) (newline) (display x))
  (list 57 321 88))
(filter
  (lambda (x) (= (remainder x 2) 0))
  squares)
(accumulate
  (lambda (x y) (+ x y))
  0
  squares)
