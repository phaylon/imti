#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/format
  racket/match)

(define entries (box '()))

(terminal-loop
  (lambda (f a)
    (render-chain f
      (lambda (f)
        (render-clear f a))
      (lambda (f)
        (render-log f a (unbox entries)))))
  (match-lambda
    ((== (key #\D (set 'control)))
     'break)
    (other
     (set-box! entries
       (append (unbox entries)
               (list (~a other))))
     'redraw)
    (_ 'redraw)))

