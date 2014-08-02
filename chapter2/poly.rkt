#lang racket

(define (attach-tag type-tag contents)
    (cons type-tag contents))

(define (install-polynomial-package)
    (define (make-poly variable term-list)
        (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    (define (variable? e) (symbol? e))
    (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (add-terms (term-list p1) (term-list p2)))
          (error "Polys not same variable -- ADD-POLY"
                 (list p1 p2))))
    
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (mul-terms (term-list p1) (term-list p2)))
          (error "Polys not same variable -- MUL-POLY"
                 (list p1 p2))))

    (define (add-terms L1 L2)
        (cond	((empty-termlist? L1) (L2))
              ((empty-termlist? L2) (L1))
              (else
                (let ((t1 (first-term L1))
                      (t2 (first-term L2)))
                  (cond	((> (order t1) (order t2))
                         (adjoin-term
                           t1 (add-terms (rest-terms L1) L2)))
                        ((< (order t1) (order t2))
                         (adjoin-term
                           t2 (add-terms L1 (rest-terms L2))))
                        (else
                          (adjoin-term
                            (make-term (order t1)
                                       (add (coeff t1) (coeff t2))))))))))

    (define (mul-terms L1 L2)
        (if (empty-termlist? L1)
          (the-empty-termlist)
          (add-terms (mul-term-by-all-terms (first-term L1) L2)
                     (mul-terms (rest-terms L1) L2))))

    (define (mul-term-by-all-terms t1 L)
        (if (empty-termlist? L)
          (the-empty-termlist)
          (let ((t2 (first-term L)))
            (adjoin-term
              (make-term (+ (order t1) (order t2))
                         (mul (coeff t1) (coeff t2)))
              (mul-term-by-all-terms t1 (rest-terms L))))))

    (define (adjoin-term t L)
        (if (= (coeff t) 0)
          L
          (cons t L)))

    (define the-empty-termlist null)
    (define (empty-termlist? L) (null? L))
    (define (first-term L) (car L))
    (define (rest-terms L) (cdr L))

    (define (make-term order coeff) (cons order coeff))
    (define (order t) (car t)) 
    (define (coeff t) (cdr t))

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add ('polynomial 'polynomial) add-poly)
    (put 'mul ('polynomial 'polynomial) mul-poly)
    (put 'make 'polynomial
         (lambda (var terms) (tag (make-poly var terms))))
    'done)

(define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))

