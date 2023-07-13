#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/list
  racket/match)

(define modes (list 'faint 'default 'bold))
(define styles
  (append
    (for*/list
      ((m modes)
       (c (list 'red 'green 'blue)))
      (make-style #:fg c #:modes (set m)))
    (for*/list
      ((m modes)
       (c (list 'bright-red 'bright-green 'bright-blue)))
      (make-style #:fg c #:modes (set m)))
    (for*/list
      ((m modes))
      (make-style #:modes (set m)))))

(terminal-loop
  (lambda (f a)
    (render-with-padding f a
      (lambda (f a)
        (let show
          ((f (render-clear f a))
           (n 0)
           (sts styles))
          (if (or (empty? sts)
                  (> n (area-width a))
                  (> n (area-height a)))
            f
            (show (render-text f
                    (subarea a n n 1 (- (area-width a) n))
                    (format
                      "Immediate Mode Terminal Interfaces in ~a and ~a mode"
                      (or (style-fg (first sts))
                          "default color")
                      (or (and (not (set-empty? (style-modes (first sts))))
                               (set-first (style-modes (first sts))))
                          "default"))
                    #:style (first sts))
                  (add1 n)
                  (rest sts)))))
      #:pad 1))
  (match-lambda
    ((== (key #\D (set 'control)))
     'break)
    (_ 'redraw)))


