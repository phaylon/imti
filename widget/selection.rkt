#lang racket/base

(require
  "../geometry.rkt"
  "../style.rkt"
  "../frame.rkt"
  "../terminal.rkt"
  "base.rkt"
  "text.rkt"
  ansi
  racket/function
  racket/list
  racket/set
  racket/match
  racket/format
  racket/contract)

(module+ test
  (require rackunit))

(struct selection-model (items selected) #:transparent)

(define (make-selection-model items (selected #f))
  (selection-model-select
    (selection-model items #f)
    selected))

(define (selection-model-length sm)
  (length (selection-model-items sm)))

(define (selection-model-empty? sm)
  (= (selection-model-length sm) 0))

(define (selection-model-ref sm index)
  (list-ref (selection-model-items sm) index))

(define (selection-model-selected-item sm)
  (cond ((selection-model-selected sm)
         =>
         (curry selection-model-ref sm))
        (else #f)))

(define (selection-model-select sm pos)
  (define index
    (cond ((or (not pos) (selection-model-empty? sm))
           #f)
          ((negative? pos)
           (max 0 (- (selection-model-length sm) (abs pos))))
          (else
           (min (sub1 (selection-model-length sm)) (abs pos)))))
  (struct-copy selection-model sm (selected index)))

(define (selection-model-select-first sm)
  (selection-model-select sm 0))

(define (selection-model-select-last sm)
  (selection-model-select sm -1))

(define (selection-model-select-next sm (wrap? #f))
  (selection-model-select sm
    (cond ((selection-model-empty? sm) #f)
          ((not (selection-model-selected sm)) 0)
          ((selection-model-at-end? sm) (if wrap? 0 -1))
          (else (add1 (selection-model-selected sm))))))

(define (selection-model-select-previous sm (wrap? #f))
  (selection-model-select sm
    (cond ((selection-model-empty? sm) #f)
          ((not (selection-model-selected sm)) -1)
          ((selection-model-at-start? sm) (if wrap? -1 0))
          (else (sub1 (selection-model-selected sm))))))

(define (selection-model-at-start? sm)
  (equal? (selection-model-selected sm) 0))

(define (selection-model-at-end? sm)
  (equal? (selection-model-selected sm)
          (sub1 (selection-model-length sm))))

(define (selection-model-update sm items)
  (selection-model-select
    (struct-copy selection-model sm (items items))
    (selection-model-selected sm)))

(module+ test
  (define (t:sm (items '()) (selected #f))
    (make-selection-model items selected))
  (check-true (selection-model-empty? (t:sm)))
  (check-equal? (selection-model-length (t:sm '(a b c))) 3)
  (check-equal? (selection-model-ref (t:sm '(a b c)) 2) 'c)
  (check-equal? (selection-model-selected-item (t:sm '(a b c) 2)) 'c)
  (check-equal? (selection-model-selected-item (t:sm '(a b c))) #f)
  (check-equal?
    (selection-model-select-first (t:sm))
    (t:sm))
  (check-equal?
    (selection-model-select-first (t:sm '(a b c)))
    (t:sm '(a b c) 0))
  (check-equal?
    (selection-model-select-last (t:sm))
    (t:sm))
  (check-equal?
    (selection-model-select-last (t:sm '(a b c)))
    (t:sm '(a b c) 2))
  (check-equal?
    (selection-model-select-next (t:sm))
    (t:sm))
  (check-equal?
    (selection-model-select-next (t:sm '(a b c)))
    (t:sm '(a b c) 0))
  (check-equal?
    (selection-model-select-next (t:sm '(a b c) 1))
    (t:sm '(a b c) 2))
  (check-equal?
    (selection-model-select-next (t:sm '(a b c) 2) #f)
    (t:sm '(a b c) 2))
  (check-equal?
    (selection-model-select-next (t:sm '(a b c) 2) #t)
    (t:sm '(a b c) 0))
  (check-equal?
    (selection-model-select-previous (t:sm))
    (t:sm))
  (check-equal?
    (selection-model-select-previous (t:sm '(a b c)))
    (t:sm '(a b c) 2))
  (check-equal?
    (selection-model-select-previous (t:sm '(a b c) 1))
    (t:sm '(a b c) 0))
  (check-equal?
    (selection-model-select-previous (t:sm '(a b c) 0) #f)
    (t:sm '(a b c) 0))
  (check-equal?
    (selection-model-select-previous (t:sm '(a b c) 0) #t)
    (t:sm '(a b c) 2))
  (check-equal?
    (selection-model-update (t:sm '(a b c) 2) '(a b))
    (t:sm '(a b) 1))
  (check-equal?
    (selection-model-update (t:sm '(a b c) 2) '())
    (t:sm '() #f)))

(provide
  (contract-out
    (struct selection-model
      ((items list?)
       (selected exact-nonnegative-integer?))
      #:omit-constructor)
    (make-selection-model
      (->* (list?) ((or/c #f exact-integer?)) selection-model?))
    (selection-model-length
      (-> selection-model? exact-nonnegative-integer?))
    (selection-model-empty?
      (-> selection-model? boolean?))
    (selection-model-ref
      (-> selection-model? exact-nonnegative-integer? any))
    (selection-model-selected-item
      (-> selection-model? any))
    (selection-model-select
      (-> selection-model? (or/c #f exact-integer?) selection-model?))
    (selection-model-select-first
      (-> selection-model? selection-model?))
    (selection-model-select-last
      (-> selection-model? selection-model?))
    (selection-model-select-next
      (->* (selection-model?) (boolean?) selection-model?))
    (selection-model-select-previous
      (->* (selection-model?) (boolean?) selection-model?))
    (selection-model-at-start?
      (-> selection-model? boolean?))
    (selection-model-at-end?
      (-> selection-model? boolean?))
    (selection-model-update
      (-> selection-model? list? selection-model?))))

(define (expand-window sm start end wanted-len)
  (if (>= (- end start) wanted-len)
    (range start end)
    (let
      ((new-start (if (= start 0) start (sub1 start)))
       (new-end (if (= end (selection-model-length sm)) end (add1 end))))
      (if (and (= start new-start) (= end new-end))
        (range start end)
        (expand-window sm new-start new-end wanted-len)))))

(define (render-selection f a sm
          #:highlight (hl "> ")
          #:highlight-style (hl-style #f)
          #:label-proc (label-proc ~a)
          #:selected-style (st-selected #f)
          #:not-selected-style (st-not-selected #f))
  (define hl-len (string-length hl))
  (define hl-empty (make-string hl-len #\space))
  (define window
    (cond
      ((selection-model-empty? sm) '())
      ((selection-model-selected sm)
       =>
       (lambda (selected)
         (expand-window sm selected (add1 selected) (area-height a))))
      (else
       (expand-window sm 0 1 (area-height a)))))
  (define (render-item f line-index item-index)
    (define selected? (equal? item-index (selection-model-selected sm)))
    (render-text
      (render-text f
        (subarea a line-index 0 1 hl-len)
        (if selected? hl hl-empty)
        #:style hl-style
        #:wrap #f)
      (subarea a line-index hl-len 1 (- (area-width a) hl-len))
      (label-proc (selection-model-ref sm item-index))
      #:style (if selected? st-selected st-not-selected)
      #:wrap #f))
  (let show-next
    ((f (render-clear f a))
     (line-index 0)
     (items window))
    (cond
      ((empty? items) f)
      (else
       (show-next
         (render-item f line-index (first items))
         (add1 line-index)
         (rest items))))))

(provide
  (contract-out
    (render-selection
      (->* (frame? area? selection-model?)
           (#:highlight string?
            #:highlight-style (or/c #f style?)
            #:label-proc (-> any/c string?)
            #:selected-style (or/c #f style?)
            #:not-selected-style (or/c #f style?))
           frame?))))

(define (selection-controller sm set-sm!
          #:wrap? (wrap? #f)
          #:key-deselect (key-deselect #f)
          #:key-next (key-next (key 'down (set)))
          #:key-previous (key-previous (key 'up (set)))
          #:key-first (key-first (key 'home (set)))
          #:key-last (key-last (key 'end (set))))
  (match-lambda
    ((== key-next)
     (set-sm! (selection-model-select-next sm wrap?)))
    ((== key-previous)
     (set-sm! (selection-model-select-previous sm wrap?)))
    ((== key-first)
     (set-sm! (selection-model-select-first sm)))
    ((== key-last)
     (set-sm! (selection-model-select-last sm)))
    ((== key-deselect)
     (set-sm! (selection-model-select sm #f)))
    (_ #f)))

(provide
  (contract-out
    (selection-controller
      (->* (selection-model?
            (-> selection-model? terminal-loop-control?))
           (#:wrap? boolean?
            #:key-deselect (or/c #f key?)
            #:key-next (or/c #f key?)
            #:key-previous (or/c #f key?)
            #:key-first (or/c #f key?)
            #:key-last (or/c #f key?))
           (-> key? (or/c #f terminal-loop-control?))))))

