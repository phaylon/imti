#lang racket/base

(require
  "../util.rkt"
  "../geometry.rkt"
  "../frame.rkt"
  racket/match
  racket/function
  racket/contract)

(define (render-paned f a e pane-proc body-proc
          #:scale (scale 1)
          #:min (min-size #f)
          #:max (max-size #f))
  (define (find-pane-size a-size)
    (define sz-full (a-size a))
    (define sz-scaled (* (/ sz-full (+ 1 scale)) scale))
    (floor
      (min (or max-size (a-size a))
           (max (or min-size 0) sz-scaled))))
  (define-values (split-o split-pos at-end?)
    (match e
      ('top
       (values 'v (find-pane-size area-height) #f))
      ('bottom
       (values 'v (- (area-height a) (find-pane-size area-height)) #t))
      ('left
       (values 'h (find-pane-size area-width) #f))
      ('right
       (values 'h (- (area-width a) (find-pane-size area-width)) #t))))
  (define-values (a-pane a-body)
    (if at-end?
      (values/reverse (thunk (area-split a split-o split-pos)))
      (area-split a split-o split-pos)))
  (chain f
    (lambda (f)
      (pane-proc f a-pane))
    (lambda (f)
      (body-proc f a-body))))

(provide
  (contract-out
    (render-paned
      (->* (frame? area? edge? (-> frame? area? frame?)
                               (-> frame? area? frame?))
           (#:scale exact-nonnegative-integer?
            #:min exact-nonnegative-integer?
            #:max exact-nonnegative-integer?)
           frame?))))

