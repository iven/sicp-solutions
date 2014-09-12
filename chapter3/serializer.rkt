#lang racket

(define (test-and-set!)
    (if (car cell)
      true
      (begin (set-mcar! cell true)
             false)))

(define (clear! cell) (set-mcar! cell false))

(define (make-mutex)
    (let ((cell (mcons false null)))
      (define (the-mutex m)
          (cond	((eq? m 'acquire)
                 (if (test-and-set! cell)
                   (the-mutex 'acquire)
                   'done))
                ((eq? m 'release) (clear! cell))))
      the-mutex))

(define (make-semaphore n)
    (let ((taken 0)
          (mutex (make-mutex)))
      (define (the-semaphore)
          (mutex 'acquire)
          (cond	((eq? m 'acquire)
                 (if (< taken n)
                   (set! taken (+ taken 1))
                   (the-semaphore 'acquire)))
                ((eq? m 'release) (set! taken (- taken 1))))
          (mutex 'release))
      the-semaphore))

(define (make-serializer)
    (let ((mutex (make-mutex)))
      (lambda (p)
        (define (serialized-p . args)
            (mutex 'acquire)
            (let ((val (apply p args)))
              (mutex 'release)))
        serialized-p)))

(module+
  main
  )
