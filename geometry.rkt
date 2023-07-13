#lang racket/base

(require
  racket/match
  racket/function
  racket/contract)

(module+ test
  (require rackunit))

(define (orientation? v)
  (or (equal? v 'v)
      (equal? v 'h)))

(define (edge? v)
  (or (equal? v 'top)
      (equal? v 'bottom)
      (equal? v 'left)
      (equal? v 'right)))

(struct position (line column) #:transparent)

(define (make-position #:line (line 0) #:column (column 0))
  (position line column))

(struct size (height width) #:transparent)

(define (make-size #:height height #:width width)
  (size height width))

(struct area (position size) #:transparent)

(define (make-area
          #:line line
          #:column column
          #:height height
          #:width width)
  (area (position line column) (size height width)))

(define (area-line a) (position-line (area-position a)))
(define (area-column a) (position-column (area-position a)))
(define (area-height a) (size-height (area-size a)))
(define (area-width a) (size-width (area-size a)))

(define (area-last-line a) (+ (area-line a) (area-height a)))
(define (area-last-column a) (+ (area-column a) (area-width a)))

(define empty-area (area (position 0 0) (size 0 0)))

(define (zero-sized-area? a)
  (or (<= (area-height a) 0)
      (<= (area-width a) 0)))

(define (position-fits-area? pos ar)
  (and (< (position-line pos) (area-height ar))
       (< (position-column pos) (area-width ar))))

(define (subarea a l c h w)
  (define new-area
    (area (position (+ (area-line a) l)
                    (+ (area-column a) c))
          (size h w)))
  (or (area-intersection a new-area)
      empty-area))

(define (area-intersection a . as)
  (define (inter/scalar start0 len0 start1 len1)
    (define start (max start0 start1))
    (define end (min (+ start0 len0) (+ start1 len1)))
    (if (> end start)
      (cons start (- end start))
      #f))
  (define (inter/area a0 a1)
    (match (cons (inter/scalar (area-line a0) (area-height a0)
                               (area-line a1) (area-height a1))
                 (inter/scalar (area-column a0) (area-width a0)
                               (area-column a1) (area-width a1)))
      ((cons (cons l h) (cons c w))
       (area (position l c) (size h w)))
      (_ #f)))
  (for/fold
    ((a0 a))
    ((a1 as) #:break (not a0))
    (inter/area a0 a1)))

(define (area-split a o pos)
  (define (split pos-last len make)
    (cond ((< pos 1) (values empty-area a))
          ((> pos (pos-last a)) (values a empty-area))
          (else
           (values
             (make 0 pos)
             (make pos (- (len a) pos))))))
  (match o
    ('v (split area-last-line area-height
          (lambda (l h) (subarea a l 0 h (area-width a)))))
    ('h (split area-last-column area-width
          (lambda (c w) (subarea a 0 c (area-height a) w))))))

(module+ test
  (define (t:a l c h w) (area (position l c) (size h w)))
  (check-equal? (area-last-line (t:a 2 10 3 10)) 5)
  (check-equal? (area-last-column (t:a 10 2 10 3)) 5)
  (check-true (zero-sized-area? empty-area))
  (check-equal?
    (area-intersection
      (t:a 2 20 5 50)
      (t:a 3 30 5 50))
    (t:a 3 30 4 40))
  (check-equal?
    (area-intersection
      (t:a 3 30 5 50)
      (t:a 2 20 5 50))
    (t:a 3 30 4 40))
  (check-equal?
    (area-intersection
      (t:a 2 20 3 30)
      (t:a 3 30 1 10))
    (t:a 3 30 1 10))
  (check-equal?
    (area-intersection
      (t:a 3 30 1 10)
      (t:a 2 20 3 30))
    (t:a 3 30 1 10))
  (check-equal?
    (area-intersection
      (t:a 2 20 3 30)
      (t:a 5 30 1 10))
    #f)
  (check-equal?
    (area-intersection
      (t:a 2 20 3 30)
      (t:a 3 50 1 10))
    #f)
  (check-equal?
    (call-with-values (thunk (area-split (t:a 2 2 4 4) 'v 2)) list)
    (list (t:a 2 2 2 4) (t:a 4 2 2 4)))
  (check-equal?
    (call-with-values (thunk (area-split (t:a 2 2 4 4) 'v 0)) list)
    (list empty-area (t:a 2 2 4 4)))
  (check-equal?
    (call-with-values (thunk (area-split (t:a 2 2 4 4) 'v 4)) list)
    (list (t:a 2 2 4 4) empty-area))
  (check-equal?
    (call-with-values (thunk (area-split (t:a 2 2 4 4) 'h 2)) list)
    (list (t:a 2 2 4 2) (t:a 2 4 4 2)))
  (check-equal?
    (call-with-values (thunk (area-split (t:a 2 2 4 4) 'h 0)) list)
    (list empty-area (t:a 2 2 4 4)))
  (check-equal?
    (call-with-values (thunk (area-split (t:a 2 2 4 4) 'h 4)) list)
    (list (t:a 2 2 4 4) empty-area)))

(provide
  (contract-out
    (orientation? (-> any/c boolean?))
    (edge? (-> any/c boolean?))
    (struct position
      ((line exact-nonnegative-integer?)
       (column exact-nonnegative-integer?)))
    (make-position
      (->* ()
           (#:line exact-nonnegative-integer?
            #:column exact-nonnegative-integer?)
           position?))
    (struct size
      ((height exact-nonnegative-integer?)
       (width exact-nonnegative-integer?)))
    (make-size
      (->* (#:height exact-nonnegative-integer?
            #:width exact-nonnegative-integer?)
           ()
           size?))
    (struct area
      ((position position?)
       (size size?)))
    (make-area
      (->* (#:line exact-nonnegative-integer?
            #:column exact-nonnegative-integer?
            #:height exact-nonnegative-integer?
            #:width exact-nonnegative-integer?)
           ()
           area?))
    (area-line (-> area? exact-nonnegative-integer?))
    (area-column (-> area? exact-nonnegative-integer?))
    (area-height (-> area? exact-nonnegative-integer?))
    (area-width (-> area? exact-nonnegative-integer?))
    (subarea (-> area?
                 exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 area?))
    (area-split
      (-> area? orientation? exact-nonnegative-integer?
          (values area? area?)))
    (zero-sized-area? (-> area? boolean?))
    (position-fits-area? (-> position? area? boolean?))))

