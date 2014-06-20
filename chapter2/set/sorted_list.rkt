#lang racket

(define (element-of-set? e s)
    (cond ((null? s) false)
          ((< e (car s)) false)
          ((equal? e (car s)) true)
          (else (element-of-set? e (cdr s)))))

(define (adjoin-set e s)
    (let ((first (car s))
          (rest (cdr s)))
      (cond	((< e first) (cons e s))
            ((= e first) s)
            (else (cons first (adjoin-set e rest))))))

(define (intersection-set s1 s2)
    (if	(or (null? s1) (null? s2))
        null
        (let ((first1 (car s1))
              (rest1 (cdr s1))
              (first2 (car s2))
              (rest2 (cdr s2)))
          (cond ((< first1 first2) (intersection-set rest1 s2))
                ((> first1 first2) (intersection-set s1 rest2))
                (else (cons first1 (intersection-set rest1 rest2)))))))

(define (union-set s1 s2)
    (cond	((null? s1) s2)
          ((null? s2) s1)
          (else
            (let ((first1 (car s1))
                  (rest1 (cdr s1))
                  (first2 (car s2))
                  (rest2 (cdr s2)))
              (cond ((< first1 first2) (cons first1 (union-set rest1 s2)))
                    ((> first1 first2) (cons first2 (union-set s1 rest2)))
                    (else (cons first1 (union-set rest1 rest2))))))))

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
