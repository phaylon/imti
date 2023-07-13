#lang racket/base

(require
  ansi
  racket/list
  racket/set
  racket/contract)

(define color-map
  `((black          . 0)
    (red            . 1)
    (green          . 2)
    (yellow         . 3)
    (blue           . 4)
    (magenta        . 5)
    (cyan           . 6)
    (white          . 7)
    (grey           . 8)
    (gray           . 8)
    (bright-red     . 9)
    (bright-green   . 10)
    (bright-yellow  . 11)
    (bright-blue    . 12)
    (bright-magenta . 13)
    (bright-cyan    . 14)
    (bright-white   . 15)))

(define (style-color? v)
  (or (and (exact-nonnegative-integer? v) (<= 0 v 255) #t)
      (and (assoc v color-map) #t)))

(define mode-map
  `((default        . 0)
    (bold           . 1)
    (faint          . 2)
    (italic         . 3)
    (italic/inverse . 3)
    (underline      . 4)
    (blink-slow     . 5)
    (blink-fast     . 6)
    (inverse        . 7)
    (conceal        . 8)
    (crossed-out    . 9)))

(define (style-mode? v)
  (and (assoc v mode-map) #t))

(struct style (fg bg modes) #:transparent)

(define (make-style #:fg (fg #f) #:bg (bg #f) #:modes (modes (set)))
  (style fg bg modes))

(define default-style (make-style))

(define (style-mode-resolve v)
  (cdr (assoc (or v 'default) mode-map)))

(define (style-color-resolve v)
  (cond ((symbol? v)
         (cdr (assoc v color-map)))
        ((exact-nonnegative-integer? v)
         v)))

(define (style-render st)
  (string-append
    (select-graphic-rendition (style-mode-resolve 'default))
    (apply string-append
      (map (compose select-graphic-rendition
                    style-mode-resolve)
           (set->list (style-modes st))))
    (cond ((style-fg st)
           =>
           (compose1 select-xterm-256-text-color
                     style-color-resolve))
          (else ""))
    (cond ((style-bg st)
           =>
           (compose1 select-xterm-256-background-color
                     style-color-resolve))
          (else ""))))

(provide
  (contract-out
    (struct style
      ((fg (or/c #f style-color?))
       (bg (or/c #f style-color?))
       (modes (set/c style-mode?))))
    (default-style style?)
    (make-style
      (->* ()
           (#:fg (or/c #f style-color?)
            #:bg (or/c #f style-color?)
            #:modes (set/c style-mode?))
           style?))
    (style-color? (-> any/c boolean?))
    (style-mode? (-> any/c boolean?))
    (style-render (-> style? string?))))

