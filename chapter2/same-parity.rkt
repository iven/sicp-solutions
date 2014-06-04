#lang racket

(define (same-parity x . l)
    (define (parity? a)
      (= (remainder (+ a x) 2) 0))
    (define (rec items)
      (if (null? items)
        null
        (let ((first (car items))
              (rest (cdr items)))
          (if (parity? first)
            (cons first (rec rest))
            (rec rest)))))
    (rec (cons x l)))

(same-parity 1 2 3 4 5 6)
(same-parity 2 3 4 5 6 7)
