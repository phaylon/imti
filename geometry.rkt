#lang racket

(require
  racket/contract)

(struct position (line column) #:transparent)

(define (make-position #:line (line 0) #:column (column 0))
  (position line column))

(struct size (height width) #:transparent)

(define (make-size #:height height #:width width)
  (size height width))

(struct area (position size) #:transparent)

(define (area-line a) (position-line (area-position a)))
(define (area-column a) (position-column (area-position a)))
(define (area-height a) (size-height (area-size a)))
(define (area-width a) (size-width (area-size a)))

(define (position-fits-area? pos ar)
  (and (< (position-line pos) (area-height ar))
       (< (position-column pos) (area-width ar))))

(provide
  (contract-out
    (struct position
      ((line exact-nonnegative-integer?)
       (column exact-nonnegative-integer?)))
    (make-position
      (->* ()
           (#:line exact-nonnegative-integer?
            #:column exact-nonnegative-integer?)
           position?))
    (struct size
      ((height exact-nonnegative-integer?)
       (width exact-nonnegative-integer?)))
    (make-size
      (->* (#:height exact-nonnegative-integer?
            #:width exact-nonnegative-integer?)
           ()
           size?))
    (struct area
      ((position position?)
       (size size?)))
    (area-line (-> area? exact-nonnegative-integer?))
    (area-column (-> area? exact-nonnegative-integer?))
    (area-height (-> area? exact-nonnegative-integer?))
    (area-width (-> area? exact-nonnegative-integer?))
    (position-fits-area? (-> position? area? boolean?))))

