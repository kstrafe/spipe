#lang racket

(require "access.rkt")

(require (for-syntax racket/function racket/list racket/pretty racket/string racket/syntax)
         (for-meta 2 racket/base)
         syntax/parse/define
         threading)

(define-syntax-parser set2 ([_ k n]                       #'(hash-set 'k k)))
(define-syntax-parser get2 ([_ w:keyword n] #'w) ([_ k n] #'(hash-ref n 'k)))

(define-syntax-parser spipe*
  ([self initial:expr term:id     rest ...]
   #'(self initial (#:x term) rest ...))
  ([_ initial:expr (kw:keyword term ...+)]
   #'(access #:surround [(state) initial]
             #:result ~>
             #:get get2
             #:set set2
             kw term ...))
  ([self initial:expr (kw:keyword term ...+) rest ...+]
   #'(access #:surround [(state) (self initial rest ...)]
             #:result ~>
             #:get get2
             #:set set2
             kw term ...))
  ([self initial:expr (term ...+) rest ...]
   #'(self initial (#:x term ...) rest ...)))
(define-syntax-parser spipe
  ([_ initial:expr xform ...+]
   #:with (rev-xform ...+) (reverse (attribute xform))
   #'(spipe* initial rev-xform ...)))
  ; THIS WORKED as single monolith
  ; ([_ initial:expr
  ;     (~and clause
  ;           (~or
  ;             (term ...)
  ;             identifier:id)
  ;           ) ...]
  ;  #:with (this ...) (last (attribute term))
  ;  ;; Drop the last syntax element
  ;  ;; E.g.: (#'a #'b #'c) -> (#'a #'b)
  ;  #:with (that ...) (take (attribute clause) (sub1 (length (attribute clause))))
  ;  ;; Prepend #:x if no other keyword is in its place
  ;  ;; E.g.: (#'f #'x ...) -> (#'#:x #'f #'x ...)
  ;  #:with (this* ...) (if (keyword? (syntax-e (car (attribute this))))
  ;                       (attribute this)
  ;                       (cons #'#:x (attribute this)))
  ;  (if (empty? (attribute that))
  ;    #'(access #:surround [(x) initial]
  ;              #:result ~>
  ;              #:get get2
  ;              #:set set2
  ;              this* ...
  ;              )
  ;    #'(access #:surround [(x) (spipe initial that ...)]
  ;              #:result ~>
  ;              #:get get2
  ;              #:set set2
  ;              this* ...))
  ;  ))

(define (f) 1)
(define (g a) a)
(define a 10)
(define (k a) (writeln a))
(define (d) (writeln 'sekai))

; (access #:x f)
(spipe (hash)
       (f #:w x)
       (add1 #:rw x)
       (g a)
       d
       )

; (spipe (hash)
;        f
;        (#:x f #:w a)
;        ; (g a #:w c)
;        ; (f #:w b)
;        ; (#:x add1 #:rw b)
;        )

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

; (define x 3129)
; (require racket/hash threading)
; (access
;   #:surround [(x) (hash 'a 100)]
;              [(o) (hash 'b 312)]
;   ; #:result ~>
;   #:get get2
;   #:set set2
;   #:x identity #:r a #:w c
;   )

; (access #:result (lambda (f) (add1 f)) #:x identity 1 #:w b)
