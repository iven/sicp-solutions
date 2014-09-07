#lang racket

(require "../queue.rkt")

(provide make-agenda empty-agenda? current-time
         the-agenda after-delay add-to-agenda!
         first-agenda-item remove-first-agenda-item!)

; Time segment
(define (make-time-segment time queue)
    (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; Agenda
(define (make-agenda) (mcons 0 null))
(define (current-time agenda) (mcar agenda))
(define (segments agenda) (mcdr agenda))

(define (set-current-time! agenda time)
    (set-mcar! agenda time))
(define (set-segments! agenda segments)
    (set-mcdr! agenda segments))

(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))

(define (empty-agenda? agenda)
    (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
        (or (null? segments)
            (< time (segment-time (mcar segments)))))

    (define (make-new-time-segment time action)
        (let ((q (make-queue)))
          (insert-queue! q action)
          (make-time-segment time q)))

    (define (add-to-segments! segments)
        (if (= (segment-time (mcar segments)) time)
          (insert-queue! (segment-queue (mcar segments))
                         action)
          (let ((rest (mcdr segments)))
            (if (belongs-before? rest)
              (set-mcdr! segments
                         (mcons (make-new-time-segment time action)
                                rest))
              (add-to-segments! rest)))))

    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
        (set-segments! agenda
                       (mcons (make-new-time-segment time action)
                              segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
      (delete-queue! q)
      (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda))
        'done)))

(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (after-delay delay action)
    (add-to-agenda! (+ (current-time the-agenda) delay)
                    action
                    the-agenda))

(define the-agenda (make-agenda))
