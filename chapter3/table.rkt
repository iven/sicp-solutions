#lang racket

(define (make-table)
    (let ((table (mcons '*table* null)))
      (define (assoc k records)
          (cond ((null? records) false)
                ((equal? k (mcar (car records))) (car records))
                (else (assoc k (cdr records)))))

      (define (lookup k)
          (let ((record (assoc k (mcdr table))))
            (if record
              (mcdr record)
              false)))

      (define (insert! k v)
          (let ((record (assoc k (mcdr table))))
            (if record
              (set-mcdr! record v)
              (set-mcdr! table
                         (cons (mcons k v)
                               (mcdr table))))))
      (define (dispatch m)
          (cond ((eq? m 'lookup) lookup)
                ((eq? m 'insert!) insert!)))
      dispatch))

(define (lookup-table k table) ((table 'lookup) k))
(define (insert-table! k v table) ((table 'insert!) k v))

(module+
  main
  (define table (make-table))
  (insert-table! 'a 1 table)
  (insert-table! 'b 2 table)
  (lookup-table 'a table)
  (insert-table! 'a 2 table)
  (lookup-table 'a table)
  (lookup-table 'b table))
