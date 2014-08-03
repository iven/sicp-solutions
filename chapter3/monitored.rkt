#lang racket

(define (make-monitored f)
    (define count 0)
    (define (mf message)
      (cond	((eq? message 'how-many-calls?) count)
            ((eq? message 'reset-count) (set! count 0))
            (else (set! count (+ count 1))
                  (f message))))
    mf)

(module+
  main
  (define s (make-monitored sqrt))
  (s 100)
  (s 'how-many-calls?))
