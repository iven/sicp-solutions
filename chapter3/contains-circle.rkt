#lang racket

(define (contains-cycle-old? x)
    (define (iter item encounted)
        (cond
          ((not (mpair? item)) false)
          ((memq item encounted) true)
          (else (iter (mcdr item) (cons item encounted)))))
    (iter x null))

(define (contains-cycle? x)
    (define (safe-mcdr l)
        (if (mpair? l) (mcdr l) null))

    (define (iter a b)
      (cond	((not (mpair? a)) false)
            ((not (mpair? b)) false)
            ((eq? a b) true)
            (else (iter (mcdr a) (safe-mcdr (safe-mcdr b))))))
    (iter x (safe-mcdr x)))

(module+
  main
  (define a1 (mcons 1 1))
  (define b1 (mcons 1 1))
  (define c1 (mcons a1 b1))
  (contains-cycle-old? c1)
  (contains-cycle? c1)

  (define a2 (mcons 1 1))
  (define b2 (mcons a2 a2))
  (set-mcdr! a2 b2)
  (contains-cycle-old? b2)
  (contains-cycle? b2))
