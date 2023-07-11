#lang racket

(require
  "util.rkt"
  "geometry.rkt"
  "style.rkt"
  racket/contract)

(module+ test
  (require rackunit))

;; elements

(struct element (style content) #:transparent)

(define (element-length elem)
  (string-length (element-content elem)))

(define (element-empty? elem)
  (= (element-length elem) 0))

(define (element-substring elem (start 0) (end (element-length elem)))
  (element (element-style elem)
           (substring (element-content elem) start end)))

(define (element-split elem pos)
  (values (element-substring elem 0 pos)
          (element-substring elem pos)))

(module+ test
  (define (t:el cnt) (element #f cnt))
  (check-equal?
    (element-length (t:el "testing"))
    7)
  (check-equal?
    (element-substring (t:el "testing") 2 4)
    (t:el "st"))
  (check-equal?
    (call-with-values
      (thunk (element-split (t:el "testing") 3))
      list)
    (list (t:el "tes")
          (t:el "ting"))))

;; frame lines

(define (line-length line-elems)
  (apply + (map element-length line-elems)))

(define (line-clean line-elems)
  (filter-not element-empty? line-elems))

(define (line-skip line-elems pos)
  (if (= pos 0)
    line-elems
    (let* ((first-elem (first line-elems))
           (first-len (element-length first-elem)))
      (if (> first-len pos)
        (cons (element-substring first-elem pos)
              (rest line-elems))
        (line-skip (rest line-elems) (- pos first-len))))))

(define (line-take line-elems pos)
  (if (= pos 0)
    '()
    (let* ((first-elem (first line-elems))
           (first-len (element-length first-elem)))
      (if (<= first-len pos)
        (cons first-elem
              (line-take (rest line-elems) (- pos first-len)))
        (list (element-substring first-elem 0 pos))))))

(define (line-insert line-elems new-elem pos)
  (line-clean
    (append
      (line-take line-elems pos)
      (list new-elem)
      (line-skip line-elems (+ pos (element-length new-elem))))))

(module+ test
  (define (t:ln . cnts) (map t:el cnts))
  (check-equal?
    (line-length (t:ln "abc" "def"))
    6)
  (check-equal?
    (line-skip (t:ln "abc" "def") 0)
    (t:ln "abc" "def"))
  (check-equal?
    (line-skip (t:ln "abc" "def") 2)
    (t:ln "c" "def"))
  (check-equal?
    (line-skip (t:ln "abc" "def") 3)
    (t:ln "def"))
  (check-equal?
    (line-skip (t:ln "abc" "def") 6)
    (t:ln))
  (check-equal?
    (line-take (t:ln "abc" "def") 0)
    (t:ln))
  (check-equal?
    (line-take (t:ln "abc" "def") 2)
    (t:ln "ab"))
  (check-equal?
    (line-take (t:ln "abc" "def") 6)
    (t:ln "abc" "def"))
  (check-equal?
    (line-insert (t:ln "abc" "def") (t:el "xy") 0)
    (t:ln "xy" "c" "def"))
  (check-equal?
    (line-insert (t:ln "abc" "def") (t:el "xy") 2)
    (t:ln "ab" "xy" "ef"))
  (check-equal?
    (line-insert (t:ln "abc" "def") (t:el "xy") 4)
    (t:ln "abc" "d" "xy")))

;; frames

(struct frame (buffer size))

(define (make-frame sz)
  (frame
    (build-list
      (size-height sz)
      (thunk* (list (element #f (make-string (size-width sz) #\space)))))
    sz))

(define (frame-height f) (size-height (frame-size f)))
(define (frame-width f) (size-width (frame-size f)))

(define (valid-frame? f)
  (and (= (length (frame-buffer f)) (frame-height f))
       (andmap
         (lambda (line-elems)
           (= (line-length line-elems) (frame-width f)))
         (frame-buffer f))))

(define (normalize-content cnt)
  ((compose
     (curryr string-replace "\n" "<nl>")
     (curryr string-replace "\r" "<cr>")
     (curryr string-replace "\t" "<tab>")
     (curryr string-replace "\v" "<vtab>"))
   cnt))

(module+ test
  (check-equal?
    (normalize-content "abc\ndef ghi\tjkl")
    "abc<nl>def ghi<tab>jkl"))

(define (frame-write f ar pos st cnt)
  (if (not (position-fits-area? pos ar))
    f
    (let* ((cnt-norm (normalize-content cnt))
           (max-len (- (area-width ar) (position-column pos)))
           (cnt-fitted (string-truncate cnt-norm max-len)))
      (struct-copy frame f
        (buffer
          (list-update
            (frame-buffer f)
            (+ (area-line ar) (position-line pos))
            (lambda (line-elems)
              (line-insert
                line-elems
                (element st cnt-fitted)
                (+ (area-column ar) (position-column pos))))))))))

(module+ test
  (define (t:fr h w) (make-frame (size h w)))
  (define (t:lines f)
    (map (lambda (line-elems)
           (apply string-append (map element-content line-elems)))
         (frame-buffer f)))
  (define f-plain (t:fr 5 10))
  (define middle (area (position 1 1) (size 3 8)))
  (check-equal?
    (t:lines f-plain)
    (list "          "
          "          "
          "          "
          "          "
          "          "))
  (check-equal?
    (t:lines (frame-write f-plain middle (position 1 3) #f "abcdefghi"))
    (list "          "
          "          "
          "    abcde "
          "          "
          "          "))
  (check-equal?
    (t:lines
      (frame-write
        (frame-write
          (frame-write f-plain middle (position 2 0) #f "after")
          middle (position 0 0) #f "before")
        middle (position 1 3) #f "abcdefghi"))
    (list "          "
          " before   "
          "    abcde "
          " after    "
          "          ")))

(provide
  (contract-out
    (struct frame
      ((buffer (listof (listof element?)))
       (size size?))
      #:omit-constructor)
    (make-frame (-> size? frame?))
    (frame-height (-> frame? exact-nonnegative-integer?))
    (frame-width (-> frame? exact-nonnegative-integer?))
    (frame-write (-> frame? area? position? style? string? frame?))))

