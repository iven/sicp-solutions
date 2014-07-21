#lang racket

(require "set/unsorted_list.rkt")

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (symbol-of-tree? element tree)
    (element-of-set-unsorted-list? element (symbols tree)))
(define (weight tree)
    (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
          null
          (let ((next-branch (choose-branch (car bits) current-branch)))
            (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (choose-branch bit tree)
    (cond	((= bit 0) (left-branch tree))
          ((= bit 1) (right-branch tree))
          (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
    (cond	((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
      null
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge leaves)
    (cond ((null? leaves) null)
          ((null? (cdr leaves)) (car leaves))
          (else (successive-merge
                  (adjoin-set (make-code-tree (car leaves) (cadr leaves))
                              (cddr leaves))))))

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(define (encode message tree)
    (if (null? message)
      null
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
    (let ((left (left-branch tree))
          (right (right-branch tree)))
      (define (encode-branch branch result)
        (if (leaf? branch)
          (list result)
          (cons result (encode-symbol symbol branch))))
      (cond ((symbol-of-tree? symbol left) (encode-branch left 0))
            ((symbol-of-tree? symbol right) (encode-branch right 1))
            (else (error "bad symbol -- ENCODE-SYMBOL" symbol)))))

(module+ main
  (define sample-tree
    (generate-huffman-tree
      (list (list 'A 4)
            (list 'B 2)
            (list 'C 1)
            (list 'D 1))))
  (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
  (define decoded-message (decode sample-message sample-tree))
  (define encoded-message (encode decoded-message sample-tree))
  sample-message
  decoded-message
  encoded-message

  (define rocktree (generate-huffman-tree '((A 2) (NA 16) (BOOM  1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))
  (define rock-song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
  (define encoded-rock-song (encode rock-song rocktree))
  encoded-rock-song)
