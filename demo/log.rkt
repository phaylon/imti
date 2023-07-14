#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/format
  racket/function
  racket/match)

(define entries '("press keys to see key log entries"))
(define (current-entries) entries)
(define (push-entry! e) (set! entries (append entries (list e))))

(define (timeout-message-evt)
  (handle-evt
    (alarm-evt (+ (current-inexact-milliseconds) 1000))
    (thunk*
      (push-entry! "no input action for one second")
      'redraw)))

(terminal-loop
  (lambda (f a)
    (render-chain f
      (lambda (f)
        (render-clear f a))
      (lambda (f)
        (render-log f a (current-entries)))))
  (lambda (k)
    (control-chain k
      (exit-controller)
      (lambda (k)
        (push-entry! (~a k))
        'redraw)))
  #:evt timeout-message-evt)

