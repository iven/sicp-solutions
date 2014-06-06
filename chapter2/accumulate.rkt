#lang racket

(provide accumulate accumulate-n flat-map)

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

(define (fold-right op initial sequence)
    (accumulate op initial sequence))

(define (fold-left op initial sequence)
    (define (iter result rest)
      (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
    (iter initial sequence))

(define (reverse-right sequence)
    (fold-right
      (lambda (first result) (append result (list first)))
      null
      sequence))

(define (reverse-left sequence)
    (fold-left
      (lambda (result first) (cons first result))
      null
      sequence))

(define (flat-map proc seq)
    (accumulate append null (map proc seq)))

(module+ main
  (define squares (list 0 1 4 9 16 25))
  (define test1 (list (list 1 2 3)
                      (list 4 5 6)
                      (list 7 8 9)
                      (list 10 11 12)))

  (fold-right (lambda (x y) (+ x y)) 0 squares)
  (fold-left / 1 (list 1 2 3))
  (accumulate-n + 0 test1)
  (reverse-right squares)
  (reverse-left squares))
