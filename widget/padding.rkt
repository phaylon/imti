#lang racket/base

(require
  "../geometry.rkt"
  "../frame.rkt"
  "base.rkt"
  racket/contract)

(define (render-with-padding f a proc
          #:pad (pad 0)
          #:h (pad-h pad)
          #:v (pad-v pad)
          #:left (pad-left pad-h)
          #:right (pad-right pad-h)
          #:top (pad-top pad-v)
          #:bottom (pad-bottom pad-v)
          #:min-height (min-height #f)
          #:min-width (min-width #f))
  (define h (- (area-height a) pad-top pad-bottom))
  (define w (- (area-width a) pad-left pad-right))
  (define a-with-v
    (if (and min-height (< h min-height))
      a
      (subarea a pad-top 0 h (area-width a))))
  (define a-with-h
    (if (and min-width (< w min-width))
      a-with-v
      (subarea a-with-v 0 pad-left (area-height a) w)))
  (proc (render-clear f a) a-with-h))

(provide
  (contract-out
    (render-with-padding
      (->* (frame? area? (-> frame? area? frame?))
           (#:pad exact-nonnegative-integer?
            #:h exact-nonnegative-integer?
            #:v exact-nonnegative-integer?
            #:left exact-nonnegative-integer?
            #:right exact-nonnegative-integer?
            #:top exact-nonnegative-integer?
            #:bottom exact-nonnegative-integer?
            #:min-height (or/c #f exact-nonnegative-integer?)
            #:min-width (or/c #f exact-nonnegative-integer?))
           frame?))))

