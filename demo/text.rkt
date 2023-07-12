#lang racket/base

(require
  "../main.rkt"
  ansi
  racket/set
  racket/list
  racket/match)

(terminal-loop
  (lambda (f a)
    (define styles
      (append
        (for*/list
          ((m (list 'faint 'default 'bold))
           (c (list 'red 'green 'blue)))
          (make-style #:fg c #:modes (set m)))
        (for*/list
          ((m (list 'faint 'default 'bold))
           (c (list 'bright-red 'bright-green 'bright-blue)))
          (make-style #:fg c #:modes (set m))))); #:fg 'black))))
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
                "Immediate Mode Terminal Interfaces"
                #:style (first sts))
              (add1 n)
              (rest sts)))))
  (match-lambda
    ((== (key #\D (set 'control)))
     'break)
    (_ 'redraw)))


