#lang racket/base

(provide spipe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/function syntax/parse/define
         lens threading
         (for-syntax racket/base racket/function racket/list racket/match racket/string racket/syntax
                     syntax/parse/define)
         (for-meta 2 racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax classes and utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-syntax

  (define-syntax-parser id-attribute->string
    ([_ id:id]
     #'(symbol->string (syntax-e (attribute id)))))

  (define-syntax-class plain-arg
    #:description "an identifier without any r/w: access specifiers"
    (pattern arg:id
             #:fail-when (regexp-match? #px"^(r:|w:|rw:|e:)[^:]+$" (id-attribute->string arg))
                         "identifier contains r/w/rw/e: prefix, illegal in this position"))

  (define-splicing-syntax-class optional-kwarg
    #:description "an identifier or a keyword followed by an identifier"
    (pattern (~seq (~optional kw:keyword) arg:plain-arg)))

  (define-splicing-syntax-class optional-read-kwarg
    #:description "an identifier or a keyword followed by an \"r:\"-prefixed identifier"
    (pattern (~seq (~optional kw:keyword) arg:id)
             #:fail-unless (regexp-match? #px"^r:[^:]+$" (id-attribute->string arg))
                           "identifier is not a proper read form \"r:[^:]+\", e.g.: \"r:name\""))

  (define-splicing-syntax-class write-arg
    #:description "an identifier or a keyword followed by a \"w:\"-prefixed identifier"
    (pattern (~seq arg:id)
             #:fail-unless (regexp-match? #px"^w:[^:]+$" (id-attribute->string arg))
                           "identifier is not a proper write form \"w:[^:]+\", e.g.: \"w:name\""))

  (define-splicing-syntax-class optional-read-write-kwarg
    #:description "an identifier or a keyword followed by an (optionally \"rw:\"-prefixed) identifier"
    (pattern (~seq (~optional kw:keyword) arg:id)
             #:fail-unless (regexp-match? #px"^(rw:)?[^:]+$" (id-attribute->string arg))
                           "identifier is not a proper read-write form \"(rw:)?[^:]+\", e.g.: \"rw:name\" or \"name\""))

  (define-splicing-syntax-class optional-external-kwarg
    #:description "an identifier or a keyword followed by an \"e:\"-prefixed identifier"
    (pattern (~seq (~optional kw:keyword) arg:id)
             #:fail-unless (regexp-match? #px"^e:[^:]+$" (id-attribute->string arg))
                           "identifier is not a proper read-write form \"e:[^:]+\", e.g.: \"e:name\""))

  (define-splicing-syntax-class optional-expression-kwarg
    #:description "an optional keyword followed by \"x:\" and an expression"
    (pattern (~seq (~optional kw:keyword) (~datum x:) arg:expr)))

  (define-splicing-syntax-class optional-xarg
    #:description "an optional keyword followed by an expression"
    (pattern (~seq (~optional kw:keyword) arg:expr)))

  (define-splicing-syntax-class args
    (pattern (~or read:optional-read-kwarg
                  write:write-arg
                  read-write:optional-read-write-kwarg
                  external:optional-external-kwarg
                  expression:optional-expression-kwarg
                  (~seq kw:keyword ((~datum  r:) ~! r-arg:plain-arg  r-kwarg:optional-kwarg  ...))
                  (~seq            ((~datum  r:) ~!                  r-kwarg:optional-kwarg  ...+))
                  (~seq            ((~datum  w:) ~! w-arg:plain-arg                          ...+))
                  (~seq kw:keyword ((~datum  e:) ~! e-arg:plain-arg  e-kwarg:optional-kwarg  ...))
                  (~seq            ((~datum  e:) ~!                  e-kwarg:optional-kwarg  ...+))
                  (~seq kw:keyword ((~datum rw:) ~! rw-arg:plain-arg rw-kwarg:optional-kwarg ...))
                  (~seq            ((~datum rw:) ~!                  rw-kwarg:optional-kwarg ...+))
                  (~seq kw:keyword ((~datum  x:) ~! x-arg:plain-arg  x-kwarg:optional-xarg ...))
                  (~seq            ((~datum  x:) ~!                  x-kwarg:optional-xarg ...+))
                  ;; Explicit illegal case
                  (~seq (~optional kw:keyword) (any (~fail (string-append "illegal head specifier" )) ~! ignore ...))
                  )))

  (define (remove-x: lst)
    (filter (lambda (x) (match (syntax-e x) ('x: #f) (_ #t))) lst))

  (define (false-merge . lsts)
    (apply map (lambda x (filter identity x)) lsts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spipe - main implementation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-parser spipe
  ([_ initial:expr
      (call:expr arguments:args ...) ...]
   #:with ((reads+ ...) ...)
     (map flatten (map false-merge
                       (attribute arguments.read)
                       (attribute arguments.read-write)
                       (attribute arguments.kw)
                       (attribute arguments.r-arg)
                       (attribute arguments.r-kwarg)
                       (attribute arguments.rw-arg)
                       (attribute arguments.rw-kwarg)))
   #:with ((reads+ext ...) ...)
     (map flatten (map false-merge
                       (attribute arguments.read)
                       (attribute arguments.read-write)
                       (attribute arguments.external)
                       (attribute arguments.expression)
                       (attribute arguments.kw)
                       (attribute arguments.r-arg)
                       (attribute arguments.r-kwarg)
                       (attribute arguments.rw-arg)
                       (attribute arguments.rw-kwarg)
                       (attribute arguments.e-arg)
                       (attribute arguments.e-kwarg)
                       (attribute arguments.x-arg)
                       (attribute arguments.x-kwarg)
                       ))
   #:with ((writes+ ...) ...)
     (map flatten (map false-merge
                       (attribute arguments.write)
                       (attribute arguments.read-write)
                       (attribute arguments.w-arg)
                       (attribute arguments.rw-arg)
                       (attribute arguments.rw-kwarg)))
   #:with ((reads+2 ...) ...) (map syntax-flatten (attribute reads+))
   #:with ((reads+3 ...) ...) (map (remove-:-prefixes "r|rw") (attribute reads+2))
   #:with ((reads+4 ...) ...) (map remove-keywords (attribute reads+3))

   #:with ((reads+ext+2 ...) ...) (map syntax-flatten (attribute reads+ext))
   #:with ((reads+ext+3 ...) ...) (map (remove-:-prefixes "e|r|rw") (attribute reads+ext+2))
   #:with ((reads+ext+4 ...) ...) (map remove-x: (attribute reads+ext+3))

   #:with ((writes+2 ...) ...) (map syntax-flatten (attribute writes+ ))
   #:with ((writes+3 ...) ...) (map (remove-:-prefixes "w|rw") (attribute writes+2))
   #:with ((writes+4 ...) ...) (map remove-keywords (attribute writes+3))

   #'(~>
         initial
         (empty-handle (reads+ ...) (writes+ ...)
           (lambda (table)
              (call table)
              table)
           (lambda (x)
             (let ([reads+4 (hash-ref-dot x reads+4 #f)] ...)
               (let-values* ([(writes+4 ...) (call reads+ext+4 ...)])
                 (~>
                   x
                   (hash-set-dot _ writes+4 writes+4) ...
                 ))))) ...)))

(module+ test
  (require rackunit)
  (test-equal? "Base case"    (spipe (hash))                             (hash))
  (test-equal? "Empty access" (spipe (hash) (identity r:read w:write))   (hash 'write #f))
  (test-equal? "Dot-access"   (spipe (hash) (identity r:a.b.c w:d.e.f))  (hash 'd (hash 'e (hash 'f #f))))
  (test-equal? "Auto-rw"      (spipe (hash 'a 1) (add1 a))               (hash 'a 2))
  (test-equal? "r: and w:"    (spipe (hash 'a 1) (add1 r:a w:b))         (hash 'a 1 'b 2))
  (test-equal? "Keyword"      (let ([f (lambda (x #:key y) (- x y))])
                                (spipe (hash 'a 1 'b 2)
                                       (f r:a #:key r:b w:c)))           (hash 'a 1 'b 2 'c -1))
  (test-equal? "Keyword-ord"  (let ([f (lambda (x #:key y) (- x y))])
                                (spipe (hash 'a 1 'b 2)
                                       (f #:key r:a r:b w:c)))           (hash 'a 1 'b 2 'c 1))
  (test-equal? "Keyword-r1"   (let ([f (lambda (x #:key y) (- x y))])
                                (spipe (hash 'a 1 'b 2)
                                       (f #:key (r: a b) w:c)))          (hash 'a 1 'b 2 'c 1))
  (test-equal? "Keyword-r2"   (let ([f (lambda (x #:key y) (- x y))])
                                (spipe (hash 'a 1 'b 2)
                                       (f (r: #:key a b) w:c)))          (hash 'a 1 'b 2 'c 1))
  (test-equal? "Keyword-r3"   (let ([f (lambda (x #:key y) (- x y))])
                                (spipe (hash 'a 1 'b 2)
                                       (f (r: a #:key b) w:c)))          (hash 'a 1 'b 2 'c -1))
  (test-equal? "rw order 1"   (spipe (hash 'a 1 'b 2) (- w:c (r: a b)))  (hash 'a 1 'b 2 'c -1))
  (test-equal? "rw order 2"   (spipe (hash 'a 1 'b 2) (- (r: a b) w:c))  (hash 'a 1 'b 2 'c -1))
  (test-equal? "rw order 3"   (spipe (hash 'a 1 'b 2) (- r:a w:c r:b))   (hash 'a 1 'b 2 'c -1))
  (test-equal? "e:"           (let ([v 123])
                                (spipe (hash)
                                       (identity e:v w:x)))              (hash 'x 123))
  (test-equal? "x: 1"         (spipe (hash) (identity x:(+ 1 2 3) w:a))  (hash 'a 6))
  (test-equal? "x: 2"         (spipe (hash) (- x: 1 x: 2 w:a))           (hash 'a -1))
  (test-equal? "(x: 1)"       (spipe (hash) (- (x: 1  2) w:a))           (hash 'a -1))
  (test-equal? "(x: 2)"       (spipe (hash) (- (x: 2  1) w:a))           (hash 'a 1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation details and utilities ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-parser empty-handle
  ([_ data () () empty-case non-empty] #'(empty-case data))
  ([_ data x  y  empty-case non-empty] #'(non-empty  data)))

(define-syntax-parser let-values*
  ([_ ([() evaluation]) body ...+]
   #'(begin evaluation body ...))
  ([_ ([bindings evaluation]) body ...+]
   #'(let-values ([bindings evaluation]) body ...)))

(define-for-syntax (remove-keywords lst)
  (filter (lambda (x) (not (keyword? (syntax-e x)))) lst))

(define-for-syntax ((remove-:-prefixes prefix) lst)
  (map (lambda (x)
         (if (symbol? (syntax-e x))
           (datum->syntax x (string->symbol (fourth (regexp-match (pregexp (string-append "^((" prefix "):)?(.+)$")) (symbol->string (syntax-e x))))) x x x)
           x)) lst))

(define-for-syntax (syntax-flatten lst)
  (flatten (map (lambda (x) (if (list? (syntax-e x)) (syntax-e x) x)) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility hash-table getter and setter ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Implements parsing of the dot-notation for accessing hash tables.

(define (hash-ref-lens* key)
  (make-lens (lambda (x  ) (hash-ref x key (make-immutable-hash)))
             (lambda (x y) (hash-set x key y))))

(define (hash-ref-nested-lens* . keys)
  (apply lens-thrush (map hash-ref-lens* keys)))

(define-syntax-parser hash-set-dot
  ([_ hash key v]
   #:with (split ...) (map (lambda (x)
                             (format-id (attribute key) "~a" x #:source (attribute key)))
                           (string-split (symbol->string (syntax-e (attribute key))) "."))
   #'(lens-set (hash-ref-nested-lens* 'split ...) hash v)))

(define-syntax-parser hash-ref-dot
  ([_ hash key failure-result]
   #:with (split ...) (map (lambda (x)
                             (format-id (attribute key) "~a" x #:source (attribute key)))
                           (string-split (symbol->string (syntax-e (attribute key))) "."))
   #'(let/ec escape
       (let ([escape* (curry escape failure-result)])
         (~>
           hash
           (hash-ref _ 'split escape*) ...)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
