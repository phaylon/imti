#lang racket/base

(require
  "../util.rkt"
  "../geometry.rkt"
  "../style.rkt"
  "../frame.rkt"
  racket/contract)

(define (render-fill f a c #:style (st #f))
  (let ((line (make-string (area-width a) c)))
    (frame-write f a (position 0 0) st line)))

(define (render-clear f a)
  (render-fill f a #\space))

(define (render-chain f . procs)
  (apply chain f procs))

(provide
  (contract-out
    (render-chain (->* (frame?) #:rest (listof (-> frame? frame?)) frame?))
    (render-fill (->* (frame? area? char?) (#:style style?) frame?))
    (render-clear (-> frame? area? frame?))))

