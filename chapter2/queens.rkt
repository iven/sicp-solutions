#lang racket

(require "accumulate.rkt")
(require "enumerate.rkt")

(define (make-queen row col) (cons row col))
(define (queen-row q) (car q))
(define (queen-col q) (cdr q))

(define empty-board null)

(define (adjoin-position row col board)
    (cons (make-queen row col) board))

(define (attackable? queen1 queen2)
    ; queen1 and queen2 are in different columns
    (or (= (queen-row queen1) (queen-row queen2))
        (= (abs (- (queen-row queen1) (queen-row queen2)))
           (abs (- (queen-col queen1) (queen-col queen2))))))

(define (safe? k positions)
    (let ((new-queen (car positions)))
      (null?
        (filter
          (lambda (queen) (attackable? queen new-queen))
          (cdr positions)))))

(define (queens board-size)
    (define (queens-cols k)
      (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queens-cols (- k 1))))))
    (queens-cols board-size))

(define (print-queens board-size)
    (let ((result (queens board-size)))
      (for-each (lambda (line)
                  (display line)
                  (newline))
                result)
      (display (length result))))

(module+
  main
  (print-queens 8))
