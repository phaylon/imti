#lang racket/base

(require
  "base.rkt"
  "../geometry.rkt"
  "../style.rkt"
  "../frame.rkt"
  racket/list
  racket/string
  racket/contract)

(define (wrap-line line max-len wrap-mode)
  (define (try-last ls)
    (and (not (empty? ls))
         (last ls)))
  (define (try-first ls)
    (and (not (empty? ls))
         (first ls)))
  (if (> (string-length line) max-len)
    (cond
      ((not wrap-mode)
       line)
      ((equal? wrap-mode 'exact)
       (cons (substring line 0 max-len)
             (wrap-line (substring line max-len) max-len wrap-mode)))
      ((equal? wrap-mode 'whitespace)
       (let ((pos
              (or (try-last (regexp-match-positions* " " line 0 max-len))
                  (try-first (regexp-match-positions* " " line)))))
         (if pos
           (let ((line-end (car pos))
                 (next-start (cdr pos)))
             (cons
               (substring line 0 line-end)
               (wrap-line
                 (substring line next-start) max-len wrap-mode)))
           (cons (substring line 0 max-len)
                 (wrap-line (substring line max-len) max-len wrap-mode))))))
    line))

(define (text-to-lines cnt max-len wrap-mode)
  (if (< max-len 0)
    (list)
    (let ((lines (string-split cnt "\n" #:trim? #f #:repeat? #f)))
      (flatten
        (map (lambda (line)
               (wrap-line (string-normalize-spaces line) max-len wrap-mode))
             lines)))))

(define (render-text-lines f a lines #:style (st #f))
  (let write-line
    ((f (render-clear f a))
     (lx 0)
     (lines lines))
    (if (or (empty? lines)
            (zero-sized-area? a))
      f
      (write-line
        (frame-write f a (position lx 0) st (first lines))
        (add1 lx)
        (rest lines)))))

(define (render-text f a cnt #:style (st #f) #:wrap (wrap 'whitespace))
  (if (zero-sized-area? a)
    f
    (render-text-lines f a
      (text-to-lines cnt (area-width a) wrap)
      #:style st)))

(define (render-text-lambda cnt #:style (st #f) #:wrap (wrap 'whitespace))
  (lambda (f a)
    (render-text f a cnt st wrap)))

(define (render-log f a entries #:style (st #f) #:wrap (wrap 'whitespace))
  (let render-next
    ((f (render-clear f a))
     (entries (reverse entries))
     (lines '())
     (line-index (sub1 (area-height a))))
    (cond
      ((negative? line-index)
       f)
      ((and (empty? lines) (empty? entries))
       f)
      ((empty? lines)
       (render-next f
         (rest entries)
         (reverse (text-to-lines (first entries) (area-width a) wrap))
         line-index))
      (else
       (render-next
         (render-text f (subarea a line-index 0 1 (area-width a))
           (first lines)
           #:wrap #f
           #:style st)
         entries
         (rest lines)
         (sub1 line-index))))))

(provide
  (contract-out
    (text-to-lines
      (-> string? exact-positive-integer? (or/c #f 'whitespace 'exact)
          (listof string?)))
    (render-log
      (->* (frame? area? (listof string?))
           (#:style (or/c #f style?)
            #:wrap (or/c #f 'whitespace 'exact))
           frame?))
    (render-text-lines
      (->* (frame? area? (listof string?))
           (#:style (or/c #f style?))
           frame?))
    (render-text
      (->* (frame? area? string?)
           (#:style (or/c #f style?)
            #:wrap (or/c #f 'whitespace 'exact))
           frame?))
    (render-text-lambda
      (->* (string?)
           (#:style (or/c #f style?)
            #:wrap (or/c #f 'whitespace 'exact))
           (-> frame? area? frame?)))))

