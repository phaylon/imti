#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/match)

(define model
  (box (make-selection-model
         '(monday
           tuesday
           wednesday
           thursday
           friday
           saturday
           sunday))))

(terminal-loop
  (lambda (f a)
    (render-selection (render-clear f a) a (unbox model)))
  (match-lambda
    ((== (key #\D (set 'control)))
     'break)
    ((== (key 'down (set)))
     (set-box! model (selection-model-select-next (unbox model)))
     'redraw)
    ((== (key 'up (set)))
     (set-box! model (selection-model-select-previous (unbox model)))
     'redraw)
    ((== (key 'down (set 'control)))
     (set-box! model (selection-model-select-next (unbox model) #t))
     'redraw)
    ((== (key 'up (set 'control)))
     (set-box! model (selection-model-select-previous (unbox model) #t))
     'redraw)
    ((== (key 'home (set)))
     (set-box! model (selection-model-select-first (unbox model)))
     'redraw)
    ((== (key 'end (set)))
     (set-box! model (selection-model-select-last (unbox model)))
     'redraw)
    (_ 'redraw)))

