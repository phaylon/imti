#lang info

(define collection "imti")
(define deps '("base" "ansi" "unix-signals"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/imti.scrbl" ())))
(define pkg-desc "Immediate Mode Terminal Interfaces")
(define version "0.0")
(define pkg-authors '(phaylon))
(define license '(Apache-2.0 OR MIT))
