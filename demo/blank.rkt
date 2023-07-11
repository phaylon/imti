#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/match)

(terminal-loop
  (lambda (f a)
    (render-clear f a))
  (match-lambda
    ((== (key #\D (set 'control)))
     'break)
    (_ 'redraw)))

