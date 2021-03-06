#lang racket

(require "../chapter1/square.rkt")

(define (attach-tag type-tag contents)
    (cons type-tag contents))

(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))
(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

(define (install-rectangular-package)
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (magnitude z)
        (sqrt (+ (square (real-part z))
                (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z)
              (real-part z)))
    (define (make-from-real-imag x y)
        (attach-tag 'rectangular (cons x y)))
    (define (make-from-mag-ang r a)
        (attach-tag 'rectangular
                    (cons (* r (cos a)) (* r (sin a)))))

    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitube '(rectangular) magnitube)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag '(rectangular)
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(rectangular)
         (lambda (x y) (tag (make-from-mag-ang x y))))
    'done)

(define (install-polar-package)
    (define (real-part z)
        (* (magnitude z) (cos (angle z))))
    (define (imag-part z)
        (* (magnitude z) (sin (angle z))))
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-real-imag x y)
        (attach-tag 'polar
                    (cons (sqrt (+ (square x) (square y)))
                          (atan y x))))
    (define (make-from-mag-ang r a)
        (attach-tag 'polar (cons r a)))

    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitube '(polar) magnitube)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag '(polar)
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(polar)
         (lambda (x y) (tag (make-from-mag-ang x y))))
    'done)

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if (proc)
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

(define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

(module+ main
  (define test1 (make-from real-imag 1 2))
  (define test2 (make-from real-imag 4 5))
  (add-complex test1 test2))
