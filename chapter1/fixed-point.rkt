#lang racket

(define tolerance 0.00001)

(define (print-line line)
    (display line)
    (newline))

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (print-line guess)
        (let ((next (f guess)))
          (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess))

(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))

(define (average-damp f)
    (lambda (x)
      (/ (+ (f x) x) 2)))

(define (average-method f guess)
    (fixed-point-of-transform f average-damp guess))

(define dx 0.00001)

(define (deriv f)
    (lambda (x)
      (/ (- (f (+ x dx)) (f x))
          dx)))

(define (newton-transform g)
    (lambda (x)
      (- x
         (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
    (fixed-point-of-transform g newton-transform guess))

(define (sqrt x) (average-method (lambda (y) (/ x y)) 1.0))
(define (sqrt-newton x) (newton-method (lambda (y) (- (* y y) x)) 1.0))

(define (cubic a b c)
    (lambda (x)
      (+ (* x x x) (* a (* x x) (* b x) c))))

(module+ main
  (print-line "SQRT")
  (sqrt 100.0)
  (newline)

  (print-line "SQRT NEWTON")
  (sqrt-newton 100.0)
  (newline)

  (print-line "E")
  (fixed-point
    (lambda (x) (+ (/ 1 x) 1))
    1.0)
  (newline)

  (print-line "E NEWTON")
  (newton-method
    (lambda (x) (+ (/ 1 x) (- 1 x)))
    1.0)
  (newline)

  (print-line "LOG(1000)/LOG(X)")
  (average-method
    (lambda (x) (/ (log 1000) (log x)))
    10.0)
  (newline))
