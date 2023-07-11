#lang racket/base

(require
  "base.rkt"
  "../geometry.rkt"
  "../style.rkt"
  "../frame.rkt"
  racket/list
  racket/string
  racket/contract)

(define (try-last ls)
  (and (not (empty? ls))
       (last ls)))

(define (try-first ls)
  (and (not (empty? ls))
       (first ls)))

(define (wrap-line line max-len wrap-mode)
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

(define (prepare-text cnt max-len wrap-mode)
  (let ((lines (string-split cnt "\n" #:trim? #f #:repeat? #f)))
    (flatten
      (map (lambda (line)
             (wrap-line (string-normalize-spaces line) max-len wrap-mode))
           lines))))

(define (render-text f a cnt #:style (st #f) #:wrap (wrap 'whitespace))
  (let write-line
    ((f (render-clear f a))
     (lx 0)
     (lines (prepare-text cnt (area-width a) wrap)))
    (if (empty? lines)
      f
      (write-line
        (frame-write f a (position lx 0) st (first lines))
        (add1 lx)
        (rest lines)))))

(provide
  (contract-out
    (render-text
      (->* (frame? area? string?)
           (#:style style? #:wrap (or/c #f 'whitespace 'exact))
           frame?))))

