#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/match)

(terminal-loop
  (lambda (f a)
    (render-clear f a))
  (lambda (k)
    (control-chain k
      (exit-controller))))

