#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/match)

(terminal-loop
  (lambda (f a)
    (render-paned (render-clear f a) a 'top
      (lambda (f a)
        (render-text f a "Top Pane Area"))
      (lambda (f a)
        (render-text f a "Body Area"))
      #:min 5 #:max 15))
  (match-lambda
    ((== (key #\D (set 'control)))
     'break)
    (_ 'redraw)))

