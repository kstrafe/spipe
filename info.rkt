#lang info
(define collection "spipe")
(define deps '("base" "nested-hash" "threading"))
(define build-deps '("sandbox-lib" "scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/spipe.scrbl" ())))
(define pkg-desc "Pipeline-based programming using hash-tables")
(define version "0.5.1")
(define pkg-authors '("Kevin R. Stravers"))
