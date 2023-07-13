#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/format
  racket/function
  racket/match)

(define entries '("press keys for log entries"))
(define (current-entries) entries)
(define (push-entry! e) (set! entries (append entries (list e))))

(terminal-loop
  (lambda (f a)
    (render-chain f
      (lambda (f)
        (render-clear f a))
      (lambda (f)
        (render-log f a (current-entries)))))
  (match-lambda
    ((== (key #\D (set 'control)))
     'break)
    (other
     (push-entry! (~a other))
     'redraw)
    (_ 'redraw))
  #:evt (thunk
          (handle-evt
            (alarm-evt (+ (current-inexact-milliseconds) 1000))
            (thunk*
              (push-entry! "no action for one second")
              'redraw))))

