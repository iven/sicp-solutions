#lang racket

(define (cont-frac n d k)
    (define (loop result count)
        (if (= count 0)
          result
          (loop (/ (n count) (+ (d count) result))
                (- count 1))))
    (loop 0 k))

(define (e-euler k)
    (+ 2
       (cont-frac
         (lambda (i) 1.0)
         (lambda (i)
           (if (= (remainder i 3) 2)
             (/ (+ i 1) 1.5)
             1))
         k)))

(define (tan-cf x k)
    (cont-frac
      (lambda (i)
        (if (= i 1)
          x
          (- (* x x))))
      (lambda (i) (- (* i 2) 1))
      k))

(module+ main
  (cont-frac
    (lambda (i) 1.0)
    (lambda (i) 1.0)
    1000)

  (e-euler 1000)

  (tan-cf 5.0 1000))
