#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/sequence
  racket/match)

(define edges (in-cycle '(top right bottom left)))
(define (current-edge) (sequence-ref edges 0))
(define (next-edge!) (set! edges (sequence-tail edges 1)))

(terminal-loop
  (lambda (f a)
    (render-paned (render-clear f a) a (current-edge)
      (lambda (f a)
        (render-text f a (symbol->string (current-edge))))
      (lambda (f a)
        (render-text f a "body"))
      #:min 5 #:max 15))
  (match-lambda
    ((== (key #\D (set 'control)))
     'break)
    ((== (key #\I (set 'control)))
     (next-edge!)
     'redraw)
    (_ 'redraw)))

