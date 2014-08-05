#lang racket

(require "../chapter1/square.rkt")

; (define rand
;     (let ((x random-init))
;       (lambda ()
;         (set! x (rand-update x))
;         x)))

(define (rand) (random 100000000))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0) (/ trials-passed trials))
              ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
              (else (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(define (cesaro-test)
    (= (gcd (rand) (rand)) 1))

(define (estimate-pi trials)
    (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (random-in-range low high)
    (+ low
       (* (random) (- high low))))

(define (circle x y)
  (<= (+ (square (- x 5))
         (square (- y 7)))
      9))

(define (estimate-integral P x1 x2 y1 y2 trials)
    (define (integral-test)
      (P (random-in-range x1 x2) (random-in-range y1 y2)))
    (let ((d (- x2 x1)))
      (/ (* (square d)
            (monte-carlo trials integral-test))
         (square (/ d 2)))))

(module+
  main
  (estimate-pi 1000000)
  (estimate-integral circle 2.0 8.0 4.0 10.0 1000000))
