#lang racket/base

(provide H~>)

(require syntax/parse/define
         (for-syntax racket/base racket/list racket/string racket/syntax)
         nested-hash threading)

(begin-for-syntax
  (define-syntax-class kwid
    (pattern
      (~or id:id
           kw:keyword))))

(define-syntax-parser nested-hash-ref*
  ([_ prev:expr ((~literal quote) access:id) default:expr]
   #:with (access* ...) (map string->symbol (string-split (symbol->string (syntax-e (attribute access))) "."))
   #'(nested-hash-ref prev 'access* ... #:default default)))

(define-syntax-parser nested-hash-set*
  ([_ prev:expr ((~literal quote) access:id) value:expr]
   #:with (access* ...) (map string->symbol (string-split (symbol->string (syntax-e (attribute access))) "."))
   #'(nested-hash-set prev 'access* ... value)))

(define-syntax-parser hash-expand
  ([_ prev:expr ((~literal H~>) substate:id transformation:expr ...)]
   #'(let ([state prev])
       (H~> state
         ((lambda (substate)
           (H~> substate
             transformation ...)
           ) substate))))
  ([_ prev:expr transform:id]
   #'(let ([state prev]) (transform state) state))
  ([_ prev:expr (transform:expr)]
   #'(let ([state prev]) (transform state) state))
  ([_ prev:expr (transform:expr (~datum *))]
   #'(let ([state prev]) (transform state)))
  ([_ prev:expr (transform:expr read-writes:kwid ...+)]
   #:with (read-writes-id ...) (filter (lambda (x) x) (attribute read-writes.id))
   #:with (read-writes-id* ...)
     (map (lambda (stx) (format-id stx "~a*" stx #:source stx))
          (attribute read-writes-id))
   #'(let* ([prev* prev]
            [read-writes-id (nested-hash-ref* prev* 'read-writes-id #f)] ...)
        (let-values ([(read-writes-id* ...) (transform read-writes ...)])
          (~>
            prev*
            (nested-hash-set* 'read-writes-id read-writes-id*) ...)
      )))
  ([_ prev:expr (transform:expr (reads:kwid ...) (~optional ()))]
   #:with (reads-id ...) (filter (lambda (x) x) (attribute reads.id))
   #'(let* ([prev* prev]
            [reads-id (nested-hash-ref* prev* 'reads-id #f)] ...)
        (begin (transform reads ...)
               prev*))
   )
  ([_ prev:expr (transform:expr (reads:kwid ...) (writes:id ...))]
   #:with (reads-id ...) (filter (lambda (x) x) (attribute reads.id))
   #'(let* ([prev* prev]
            [reads-id (nested-hash-ref* prev* 'reads-id #f)] ...)
        (let-values ([(writes ...) (transform reads ...)])
          (~>
            prev*
            (nested-hash-set* 'writes writes) ...)))
   )
  )

(define-syntax-parser H~>
  ([_ init:expr terms:expr ...]
   #:with (term/hash-expand ...)
     (for/list ([term (in-list (attribute terms))])
       (quasisyntax/loc term (hash-expand #,term)))
   #'(~> init term/hash-expand  ...)))
