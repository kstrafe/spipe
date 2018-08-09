#lang racket

(require "test.rkt")

(define (f x)
  (writeln x))

(spipe
  (hash)
  ((const 0) #:w  value)
  (add1      #:rw value)
  (f #:r ())
  (add1 #:rw value)
  (sqr #:rw value)
  )
