#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/function
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
    (render-with-padding f a
      (lambda (f a)
        (render-paned f a 'top
          (lambda (f a)
            (render-text f a
              (format "selected: ~a"
                (selection-model-selected-item (unbox model)))))
          (lambda (f a)
            (render-selection (render-clear f a) a (unbox model)
              #:selected-style (make-style #:modes (set 'bold))))
          #:min 1 #:max 2))
      #:pad 1 #:h 4 #:min-height 7))
  (lambda (k)
    (control-chain k
      (exit-controller)
      (selection-controller
        (unbox model)
        (lambda (new-model)
          (set-box! model new-model)
          'redraw)
        #:key-deselect (key 'backspace (set))))))

