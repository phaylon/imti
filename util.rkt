#lang racket/base

(require
  racket/contract)

(define (string-truncate str len)
  (if (> (string-length str) len)
    (substring str 0 len)
    str))

(define (values/reverse proc)
  (apply values (reverse (call-with-values proc list))))

(define (chain v . procs)
  (for/fold ((v v)) ((proc procs))
    (proc v)))

(provide
  (contract-out
    (chain (->* (any/c) #:rest (listof procedure?) any))
    (values/reverse (-> procedure? any))
    (string-truncate (-> string? exact-nonnegative-integer? string?))))

