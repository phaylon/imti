#lang racket

(require
  ansi
  racket/contract)

(define base/fg 30)
(define base/bg 40)
(define base/mode 0)

(define color-map
  `((black          . 0)
    (red            . 1)
    (green          . 2)
    (yellow         . 3)
    (blue           . 4)
    (magenta        . 5)
    (cyan           . 6)
    (white          . 7)
    (default        . 9)
    (bright-black   . 60)
    (bright-red     . 61)
    (bright-green   . 62)
    (bright-yellow  . 63)
    (bright-blue    . 64)
    (bright-magenta . 65)
    (bright-cyan    . 66)
    (bright-white   . 67)))

(define (style-color? v)
  (and (assoc v color-map)
       #t))

(define mode-map
  `((default        . 0)
    (bold           . 1)
    (faint          . 2)
    (italic/inverse . 3)
    (underline      . 4)
    (blink-slow     . 5)
    (blink-fast     . 6)
    (inverse        . 7)
    (conceal        . 8)
    (crossed-out    . 9)))

(define (style-mode? v)
  (and (assoc v mode-map)
       #t))

(struct style (fg bg modes) #:transparent)

(define (make-style
          #:fg (fg #f)
          #:bg (bg #f)
          #:modes (modes '()))
  (style fg bg modes))

(define (style-map-resolve tbl v base)
  (+ base (cdr (assoc (or v 'default) tbl))))

(define (style-render st)
  (define codes
    (flatten
      (list (style-map-resolve mode-map 'default base/mode)
            (map (lambda (mode)
                   (style-map-resolve mode-map mode base/mode))
                 (set->list (style-modes st)))
            (style-map-resolve color-map (style-fg st) base/fg)
            (style-map-resolve color-map (style-bg st) base/bg))))
  (apply string-append (map select-graphic-rendition codes)))

(provide
  (contract-out
    (struct style
      ((fg style-color?)
       (bg style-color?)
       (modes (set/c style-mode?))))
    (make-style
      (->* ()
           (#:fg style-color?
            #:bg style-color?
            #:modes (set/c style-mode?))
           style?))
    (style-color? (-> any/c boolean?))
    (style-mode? (-> any/c boolean?))
    (style-render (-> style? string?))))

