#lang racket

(require scheme/mpair)

(define (front-ptr q) (mcar q))
(define (rear-ptr q) (mcdr q))
(define (set-front-ptr! q item) (set-mcar! q item))
(define (set-rear-ptr! q item) (set-mcdr! q item))
(define (empty-queue? q) (null? (front-ptr q)))
(define (make-queue) (mcons null null))

(define (front-queue q)
    (if (empty-queue? q)
      (error "FRONT called with an empty queue" q)
      (front-ptr q)))

(define (insert-queue! q item)
    (let ((new-pair (mcons item null)))
      (if (empty-queue? q)
        (begin
          (set-front-ptr! q new-pair)
          (set-rear-ptr! q new-pair)
          q)
        (begin
          (set-rear-ptr! (rear-ptr q) new-pair)
          (set-rear-ptr! q new-pair)
          q))))

(define (delete-queue! q)
    (if (empty-queue? q)
      (error "DELETE! called with an empty queue" q)
      (begin
        (set-front-ptr! q (rear-ptr (front-ptr q)))
        q)))

(define (print-queue q) (mcar q))

(module+
  main
  (define q1 (make-queue))
  (print-queue (insert-queue! q1 'a))
  (print-queue (insert-queue! q1 'b))
  (print-queue (delete-queue! q1))
  (print-queue (delete-queue! q1)))
