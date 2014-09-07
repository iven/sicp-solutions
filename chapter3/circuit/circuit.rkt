#lang racket

(require "agenda.rkt")
(require "boxes.rkt")
(require "wire.rkt")

(define (propagate)
    (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
    (define (show)
        (display name)
        (display " ")
        (display (current-time the-agenda))
        (display " New-value = ")
        (display (get-signal wire))
        (newline))
    
    (add-action! wire show))

(module+
  main
  (define input-1 (make-wire))
  (define input-2 (make-wire))
  (define sum (make-wire))
  (define carry (make-wire))

  (probe 'sum sum)
  (probe 'carry carry)
  (half-adder input-1 input-2 sum carry)
  (set-signal! input-1 1)
  (propagate)
  (set-signal! input-2 1)
  (propagate))
