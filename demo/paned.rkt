#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/sequence
  racket/match)

(define model (box (make-cycle-model 'top 'right 'bottom 'left)))
(define show? (box (make-flag-model #t)))

(terminal-loop
  (lambda (f a)
    (render-paned (render-clear f a) a
      (cycle-model-selected (unbox model))
      (lambda (f a)
        (render-text f a
          (string-append
            (symbol->string (cycle-model-selected (unbox model)))
            "\npane")))
      (lambda (f a)
        (render-text f a
          (string-append
            "body area\n"
            (format "pane position: ~a (tab to cycle)\n"
              (cycle-model-selected (unbox model)))
            (format "pane visible: ~a (ctrl-v to toggle)\n"
              (if (flag-model-on? (unbox show?)) "yes" "no")))))
      #:min 5 #:max 15 #:show-pane? (flag-model-on? (unbox show?))))
  (lambda (k)
    (control-chain k
      (exit-controller)
      (flag-controller (unbox show?)
        (lambda (new-flag)
          (set-box! show? new-flag)
          'redraw)
        #:key-toggle (key #\V (set 'control)))
      (cycle-controller (unbox model)
        (lambda (new-model)
          (set-box! model new-model)
          'redraw)
        #:key-next (key #\I (set 'control))
        #:key-previous (key #\I (set 'control 'shift))))))

