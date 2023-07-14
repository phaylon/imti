#lang racket/base

(require
  "../util.rkt"
  "../geometry.rkt"
  "../style.rkt"
  "../frame.rkt"
  "../terminal.rkt"
  ansi
  racket/set
  racket/vector
  racket/match
  racket/contract)

(define (render-fill f a c #:style (st #f))
  (let ((line (make-string (area-width a) c)))
    (frame-write f a (position 0 0) st line)))

(define (render-clear f a)
  (render-fill f a #\space))

(define (render-chain f . procs)
  (apply chain f procs))

(define (control-chain k . procs)
  (ormap (lambda (proc)
           (proc k))
         procs))

(define (exit-controller
          #:ctrl-d? (ctrl-d #t)
          #:ctrl-q? (ctrl-q #t)
          #:ctrl-c? (ctrl-c #f))
  (lambda (k)
    (cond ((or (and ctrl-d (equal? k (key #\D (set 'control))))
               (and ctrl-q (equal? k (key #\Q (set 'control))))
               (and ctrl-c (equal? k (key #\C (set 'control)))))
           'break)
          (else #f))))

(struct cycle-model (items) #:transparent)

(define (make-cycle-model . items)
  (cycle-model (list->vector items)))

(define (cycle-model-empty? cm)
  (vector-empty? (cycle-model-items cm)))

(define (cycle-model-length cm)
  (vector-length (cycle-model-items cm)))

(define (cycle-model-selected cm)
  (cond ((cycle-model-empty? cm) #f)
        (else (vector-ref (cycle-model-items cm) 0))))

(define (cycle-model-select-next cm)
  (cond ((cycle-model-empty? cm) cm)
        (else
         (define items (cycle-model-items cm))
         (cycle-model
           (build-vector (vector-length items)
             (lambda (n)
               (cond ((= n (sub1 (vector-length items)))
                      (vector-ref items 0))
                     (else
                      (vector-ref items (add1 n))))))))))

(define (cycle-model-select-previous cm)
  (cond ((cycle-model-empty? cm) cm)
        (else
          (define items (cycle-model-items cm))
          (cycle-model
            (build-vector (vector-length items)
              (lambda (n)
                (cond ((= n 0)
                       (vector-ref items (sub1 (vector-length items))))
                      (else
                       (vector-ref items (sub1 n))))))))))

(define (cycle-controller cm set-cm!
          #:key-next key-next
          #:key-previous key-previous)
  (match-lambda
    ((== key-next)
     (set-cm! (cycle-model-select-next cm)))
    ((== key-previous)
     (set-cm! (cycle-model-select-previous cm)))
    (else #f)))

(struct flag-model (on?) #:transparent)

(define (make-flag-model (on? #f))
  (flag-model on?))

(define (flag-model-toggle fm)
  (flag-model (not (flag-model-on? fm))))

(define (flag-controller fm set-fm!
          #:key-toggle key-toggle)
  (match-lambda
    ((== key-toggle)
     (set-fm! (flag-model-toggle fm)))
    (else #f)))

(provide
  (contract-out
    (render-chain (->* (frame?) #:rest (listof (-> frame? frame?)) frame?))
    (render-fill (->* (frame? area? char?) (#:style style?) frame?))
    (render-clear (-> frame? area? frame?))
    (control-chain
      (->* (key?)
           #:rest (listof (-> key? (or/c #f terminal-loop-control?)))
           (or/c #f terminal-loop-control?)))
    (struct cycle-model ((items vector?)))
    (make-cycle-model (->* () #:rest list? cycle-model?))
    (cycle-model-length (-> cycle-model? exact-nonnegative-integer?))
    (cycle-model-empty? (-> cycle-model? boolean?))
    (cycle-model-selected (-> cycle-model? any))
    (cycle-model-select-next (-> cycle-model? cycle-model?))
    (cycle-model-select-previous (-> cycle-model? cycle-model?))
    (cycle-controller
      (->* (cycle-model?
            (-> cycle-model? (or/c #f terminal-loop-control?))
            #:key-next (or/c #f key?)
            #:key-previous (or/c #f key?))
           (-> key? (or/c #f terminal-loop-control?))))
    (struct flag-model ((on? boolean?)))
    (make-flag-model (->* () (boolean?) flag-model?))
    (flag-model-toggle (-> flag-model? flag-model?))
    (flag-controller
      (->* (flag-model?
            (-> flag-model? (or/c #f terminal-loop-control?))
            #:key-toggle (or/c #f key?))
           (-> key? (or/c #f terminal-loop-control?))))
    (exit-controller
      (->* ()
           (#:ctrl-d? boolean?
            #:ctrl-q? boolean?
            #:ctrl-c? boolean?)
           (-> key? (or/c #f terminal-loop-control?))))))


