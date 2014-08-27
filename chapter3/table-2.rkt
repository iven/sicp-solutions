#lang racket

(define (make-table)
    (let ((table (mcons '*table* null)))
      (define (assoc k records)
          (cond ((null? records) false)
                ((equal? k (mcar (car records))) (car records))
                (else (assoc k (cdr records)))))

      (define (lookup k1 k2)
          (let ((subtable (assoc k1 (mcdr table))))
            (if subtable
              (let ((record (assoc k2 (mcdr subtable))))
                (if record
                  (mcdr record)
                  (false)))
              false)))

      (define (insert! k1 k2 v)
          (let ((subtable (assoc k1 (mcdr table))))
            (if subtable
              (let ((record (assoc k2 (mcdr table))))
                (if record
                  (set-mcdr! record v)
                  (set-mcdr! subtable
                             (cons (mcons k2 v)
                                   (mcdr subtable)))))
              (set-mcdr! table
                         (cons (mcons k1 (cons (mcons k2 v) null))
                               (mcdr table))))))
      (define (dispatch m)
          (cond ((eq? m 'lookup) lookup)
                ((eq? m 'insert!) insert!)))
      dispatch))

(define (lookup-table k1 k2 table) ((table 'lookup) k1 k2))
(define (insert-table! k1 k2 v table) ((table 'insert!) k1 k2 v))

(module+
  main
  (define table (make-table))
  (insert-table! 'x 'a 1 table)
  (insert-table! 'x 'b 2 table)
  (lookup-table 'x 'a table)
  (insert-table! 'x 'a 2 table)
  (lookup-table 'x 'a table)
  (lookup-table 'x 'b table))
