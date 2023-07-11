#lang racket/base

(require
  racket/contract)

(define (string-truncate str len)
  (if (> (string-length str) len)
    (substring str 0 len)
    str))

(provide
  (contract-out
    (string-truncate (-> string? exact-nonnegative-integer? string?))))

