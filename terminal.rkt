#lang racket/base

(require
  "geometry.rkt"
  "frame.rkt"
  ansi
  unix-signals
  racket/list
  racket/port
  racket/function
  racket/match
  racket/set
  racket/contract)

(define (terminal-push . vs)
  (for-each display vs)
  (flush-output))

(define (terminal-present f)
  (let present-next
    ((lines (frame-render f))
     (line-number 1))
    (unless (empty? lines)
      (terminal-push (goto line-number 1) (first lines))
      (present-next (rest lines) (add1 line-number)))))

(define (error-protect proc)
  (let/ec return/success
    (raise
      (let/ec return/failure
        (call-with-exception-handler
          return/failure
          (thunk (return/success (proc))))))))

(define (prepare-signals user-signals)
  (list* (cons 'SIGWINCH
               (thunk (terminal-push (device-request-screen-size))
                      'ignore))
         user-signals))

(define (terminal-loop
          draw-proc
          input-proc
          #:escape-timeout (esc-timeout 50)
          #:evt (user-evt-proc (thunk never-evt))
          #:signals (user-signals '()))
  ; state
  (define last-screen-size-report (box #f))
  ; handlers
  (define signals (prepare-signals user-signals))
  (define (any-bytes-evt (n 1))
    (peek-bytes-evt n 0 #f (current-input-port)))
  (define (try-redraw)
    (define (redraw ssr)
      (let* ((height (screen-size-report-rows ssr))
             (width (screen-size-report-columns ssr))
             (sz (size height width))
             (a (area (position 0 0) sz))
             (f (make-frame sz)))
        (terminal-present (draw-proc f a))
        #t))
    (cond ((unbox last-screen-size-report) => redraw)
          (else #f)))
  (define (handle-signal-evt signal)
    (cond ((assoc (lookup-signal-name signal) signals)
           =>
           (lambda (handler) ((cdr handler))))
          (else 'ignore)))
  (define (handle-input-available-evt v)
    (cond
      ((equal? (bytes->list v) (list 27))
       (sync
         (handle-evt
          (any-bytes-evt 2)
           handle-input-available-evt)
         (handle-evt
           (alarm-evt (+ (current-inexact-milliseconds) esc-timeout))
           (thunk*
             (read-byte (current-input-port))
             (input-proc (key 'escape (set)))
             'redraw))))
      (else
       (match (lex-lcd-input (current-input-port))
         ((? eof-object?)
          'break)
         ((? screen-size-report? ssr)
          (set-box! last-screen-size-report ssr)
          'redraw)
         ((? key? k)
          (input-proc k))
         (_ 'ignore)))))
  ; terminal env
  (define (terminal-setup)
    (tty-raw!)
    (for-each capture-signal! (map car signals))
    (terminal-push
      (dec-soft-terminal-reset)
      (dec-save-cursor)
      (hide-cursor)
      (set-mode alternate-screen-buffer-mode)
      (device-request-screen-size))
    (clear-screen)
    (try-redraw))
  (define (terminal-shutdown)
    (clear-screen)
    (terminal-push
      (reset-mode alternate-screen-buffer-mode)
      (show-cursor)
      (dec-restore-cursor)
      (dec-soft-terminal-reset))
    (for-each release-signal! (map car signals))
    (tty-restore!))
  ; main loop
  (define (terminal-inner-loop)
    (let continue ()
      (terminal-push
        (dec-soft-terminal-reset))
      (define op
        (sync (user-evt-proc)
              (handle-evt next-signal-evt handle-signal-evt)
              (handle-evt (any-bytes-evt) handle-input-available-evt)))
      (match op
        ('break
         (void))
        ((or #f 'ignore)
         (continue))
        ('redraw
         (try-redraw)
         (continue)))))
  (error-protect
    (thunk
      (dynamic-wind
        terminal-setup
        terminal-inner-loop
        terminal-shutdown))))

(define (terminal-loop-control? v)
  (or (equal? v 'break)
      (equal? v 'ignore)
      (equal? v 'redraw)))

(provide
  (contract-out
    (terminal-loop-control?
      (-> any/c boolean?))
    (terminal-loop
      (->* ((-> frame? area? frame?)
            (-> key? (or/c #f terminal-loop-control?)))
           (#:evt (-> (evt/c terminal-loop-control?))
            #:signals (listof (cons/c symbol? (-> terminal-loop-control?)))
            #:escape-timeout exact-positive-integer?)
           any))))

