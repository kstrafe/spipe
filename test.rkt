#lang racket

(require "access.rkt")

(require (for-syntax racket/function racket/list racket/pretty racket/string racket/syntax)
         (for-meta 2 racket/base)
         syntax/parse/define
         threading)

(define-syntax-parser spipe-set ([_ k n]                       #'(hash-set 'k  k)))
(define-syntax-parser spipe-get ([_ w:keyword n] #'w) ([_ k n] #'(hash-ref  n 'k)))

(define-syntax-parser spipe*
  ([self initial:expr term:id     rest ...]
   #'(self initial (#:x term) rest ...))
  ([_ initial:expr (kw:keyword term ...+)]
   #'(access #:surround [(state) initial]
             #:result ~>
             #:get spipe-get
             #:set spipe-set
             kw term ...))
  ([self initial:expr (kw:keyword term ...+) rest ...+]
   #'(access #:surround [(state) (self initial rest ...)]
             #:result ~>
             #:get spipe-get
             #:set spipe-set
             kw term ...))
  ([self initial:expr (term ...+) rest ...]
   #'(self initial (#:x term ...) rest ...)))

(define-syntax-parser spipe
  ([_ initial:expr xform ...+]
   #:with (rev-xform ...+) (reverse (attribute xform))
   #'(spipe* initial rev-xform ...)))

(define (f) 1)
(define (g a) (writeln `(it works ,a)) a)
(define a 10)
(define (k a) (writeln a))
(define (d) (writeln 'sekai))
(define (kwt #:r r #:w w) (* r w))

(spipe (hash)
       (f              #:w  x)
       (add1           #:rw x)
       (g (+ 1 2 a) #:w test)
       (kwt #:x #:r (+ 1 2 3) #:x #:w (- 1 2 4) #:w ZE)
       d
       )

(define-syntax (s a)
  (parameterize ([print-syntax-width +inf.0])
    (pretty-print
      (syntax->datum (local-expand
        (cadr (syntax-e a))
        'expression
        '()))))
  #'(void))


(s (spipe (hash)
             (f #:w x)
             (add1 #:rw x)
             (+ 3 #:rw x)
             ))
