#lang racket

;;;; First iteration of spipe, the universal programming pipeline

(provide spipe)

(require syntax/parse
         syntax/parse/define
         lens threading
         (for-syntax racket/base racket/list racket/string racket/syntax
                     threading))

(define-for-syntax ((is-prefixed-datum? prefix-regexp) stx)
  (define dtm (syntax->datum stx))
  (and (list? dtm)
       (> (length dtm) 1)
       (symbol? (first dtm))
       (regexp-match? (pregexp (string-append "^" prefix-regexp ":$")) (symbol->string (first dtm)))))

(define-for-syntax ((collect-identifiers prefix-regexp #:keywords-included? kw-include) arguments)
  (define is-desired-datum? (is-prefixed-datum? prefix-regexp))
  (map
    (lambda (x)
      (flatten
        (map
          (lambda (x)
            (cond
              [(identifier? x)     (let ([n (regexp-match (pregexp (string-append "^(" prefix-regexp ":(.+)|([^:]+))$")) (symbol->string (syntax->datum x)))])
                                     (datum->syntax x (string->symbol (or (third n) (fourth n))) x))]
              [(is-desired-datum? x) (rest (syntax-e x))]
              [else                x]))
          (filter
            (lambda (x)
              (cond
                [(identifier? x)     (regexp-match? (pregexp (string-append "^(" prefix-regexp ":.+|[^:]+)$")) (symbol->string (syntax->datum x)))]
                [(keyword? (syntax-e x)) kw-include]
                [(is-desired-datum? x) #t]
                [else                #f]))
            x))))
    arguments))

(define-for-syntax collect-writes (collect-identifiers "r?w" #:keywords-included? #f))
(define-for-syntax collect-reads  (collect-identifiers "rw?" #:keywords-included? #t))

(define-syntax-parser define-values*
  ([_ () y] #'y)
  ([_ x  y] #'(define-values x y)))

(define-syntax-parser this-or-just-x
  ([_ x] #'x)
  ([z x (set str ino w) ...+] #'(~> x (set str _ w) ...)))

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

; (define-for-syntax (nested-construct hash key keys v)
;   (if (empty? keys)
;     #`(lens-set (hash-ref-lens #,key) #,hash #,v)
;     #`(begin
;         (lens-set (hash-ref-lens #,key) #,hash
;         )))

(define (hash-ref-lens* key)
  (make-lens (lambda (x) (hash-ref x key (hash)))
             (lambda (x y) (hash-set x key y))))

(define (hash-ref-nested-lens* . keys)
  (apply lens-thrush (map hash-ref-lens* keys)))

(define-syntax-parser hash-set-dot
  ([_ hash key v]
   #:with (split ...) (map (lambda (x)
                             (format-id (attribute key) "~a" x #:source (attribute key)))
                           (string-split (symbol->string (syntax-e (attribute key))) "."))
   #'(lens-set (hash-ref-nested-lens* 'split ...) hash v)))

(require (for-syntax racket/function))

(define-syntax-parser let-values*
  ([_ ([() evaluation]) body ...+]
   #'(begin evaluation body ...))
  ([_ ([bindings evaluation]) body ...+]
   #'(let-values ([bindings evaluation]) body ...)))

;; TODO use these
(begin-for-syntax
  ; arguments => name
  ;              kw name | optional-kwarg
  ;              kw (sp optional-kwarg)

  (define-splicing-syntax-class optional-kwarg
    #:description "an identifier or a keyword followed by an identifier"
    (pattern (~seq (~optional kw:keyword) arg:id)))

  (define-splicing-syntax-class args
    (pattern (~or singular:optional-kwarg
                  (~seq (~optional kw:keyword) ((~datum  r:) rarg:id kwarg:optional-kwarg ...))
                  (~seq                        ((~datum  w:) arg:id               ...+))
                  (~seq (~optional kw:keyword) ((~datum rw:) kwarg:optional-kwarg ...+))))))

(define-syntax-parser spipe
  ([_ initial:expr
      (call:expr arguments ...) ...]
   #:with ((writes ...) ...) (collect-writes (attribute arguments))
   #:with ((reads  ...) ...) (collect-reads (attribute arguments))
   #:with ((reads-no-kwargs ...) ...) (map (lambda (x) (filter (lambda (y) (not (keyword? (syntax-e y)))) x))
                                           (attribute reads))
   #:with ((reads* ...) ...) (map (curryr remove-duplicates free-identifier=?)
                                  (attribute reads-no-kwargs))
   (writeln (attribute arguments))
   (writeln (attribute reads))
   #'(~>
         initial
         ((lambda (x)
           (let ([reads* (hash-ref-dot x reads* #f)] ...)
             (let-values* ([(writes ...) (call reads ...)])
               (~>
                 x
                 (hash-set-dot _ writes writes) ...
               ))))) ...
      )))
