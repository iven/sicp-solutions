#lang racket

(require "agenda.rkt")
(require "wire.rkt")

(provide logical-and logical-or logical-not
         and-gate or-gate or-gate-alternative inverter
         half-adder full-adder ripple-carry-adder)

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (logical-and a1 a2)
    (cond ((and (= a1 1) (= a2 1)) 1)
          ((and (or (= a1 0) (= a1 1))
                (or (= a2 0) (= a2 1)))
           0)
          (else (error "Invalid signal -- LOGICAL-AND" a1 a2))))

(define (logical-or a1 a2)
    (cond ((and (= a1 0) (= a2 0)) 0)
          ((and (or (= a1 0) (= a1 1))
                (or (= a2 0) (= a2 1)))
           1)
          (else (error "Invalid signal -- LOGICAL-OR" a1 a2))))

(define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal -- LOGICAL-NOT" s))))

(define (and-gate a1 a2 output)
    (define (and-action-procedure)
        (let ((new-value (logical-and (get-signal a1)
                                      (get-signal a2))))
          (after-delay and-gate-delay
                       (lambda ()
                         (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure))

(define (or-gate a1 a2 output)
    (define (or-action-procedure)
        (let ((new-value (logical-or (get-signal a1)
                                     (get-signal a2))))
          (after-delay or-gate-delay
                       (lambda ()
                         (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure))

(define (or-gate-alternative a1 a2 output)
    (let ((c1 (make-wire))
          (c2 (make-wire))
          (c3 (make-wire)))
      (inverter a1 c1)
      (inverter a2 c2)
      (and-gate c1 c2 c3)
      (inverter c3 output)))

(define (inverter input output)
    (define (invert-input)
        (let ((new-value (logical-not (get-signal input))))
          (after-delay inverter-delay
                       (lambda ()
                         (set-signal! output new-value)))))
    (add-action! input invert-input))

(define (half-adder a b s c)
    (let ((d (make-wire))
          (e (make-wire)))
      (or-gate a b d)
      (and-gate a b c)
      (inverter c e)
      (and-gate d e s)
      'ok))

(define (full-adder a b c-in sum c-out)
    (let ((s (make-wire))
          (c1 (make-wire))
          (c2 (make-wire)))
      (half-adder b c-in s c1)
      (half-adder a s sum c2)
      (or-gate c1 c2 c-out)
      'ok))

(define (ripple-carry-adder a b s c)
    (let ((c-in (make-wire)))
      (if (null? (cdr a))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) (c-in)))
      (full-adder (car a) (car b) c-in (car s) c)))
