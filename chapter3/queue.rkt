#lang racket

(require scheme/mpair)

(define (make-queue)
    (let ((front-ptr null)
          (rear-ptr null))
      (define (set-front-ptr! value)
          (set! front-ptr value)
          dispatch)
      (define (set-rear-ptr! value)
          (set! rear-ptr value)
          dispatch)
      (define (empty?) (null? front-ptr))

      (define (insert! item)
          (let ((new-pair (mcons item null)))
            (if (empty?)
              (begin
                (set-front-ptr! new-pair)
                (set-rear-ptr! new-pair))
              (begin
                (set-mcdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)))))

      (define (delete!)
          (if (empty?)
            (error "DELETE! called with an empty queue")
            (set-front-ptr! (mcdr front-ptr))))

      (define (print) front-ptr)

      (define (dispatch m)
          (cond ((eq? m 'front) front-ptr)
                ((eq? m 'rear) rear-ptr)
                ((eq? m 'insert!) insert!)
                ((eq? m 'delete!) delete!)
                ((eq? m 'print) print)))
      dispatch))

(define (front-queue q) ((q 'front)))
(define (rear-queue q) ((q 'rear)))
(define (insert-queue! q item) ((q 'insert!) item))
(define (delete-queue! q) ((q 'delete!)))
(define (print-queue q)
    (if (null? q)
      null
      ((q 'print))))


(module+
  main
  (define q1 (make-queue))
  (print-queue (insert-queue! q1 'a))
  (print-queue (insert-queue! q1 'b))
  (print-queue (delete-queue! q1))
  (print-queue (delete-queue! q1)))
