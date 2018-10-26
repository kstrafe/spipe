#lang racket/base

(provide define/H~> H~>)

(require syntax/parse/define
         racket/list
         (for-syntax racket/base racket/list racket/string racket/syntax)
         nested-hash threading)

(begin-for-syntax
  (define-syntax-class kwid
    (pattern
      (~or id:id
           kw:keyword)))
  (define (dotted->list-of-ids identifier)
    (map string->symbol (string-split (symbol->string (syntax-e identifier)) "."))))

(define-syntax-parser nested-hash-ref*
  ([_ prev:expr ((~literal quote) access:id) default:expr]
   #:with (access* ...) (dotted->list-of-ids (attribute access))
   #'(nested-hash-ref prev 'access* ... #:default default)))

(define-syntax-parser nested-hash-set*
  ([_ prev:expr ((~literal quote) access:id) value:expr]
   #:with (access* ...) (dotted->list-of-ids (attribute access))
   #'(nested-hash-set prev #:hash hasheq 'access* ... value)))

(define-syntax-parser let-values/empty
  ([_ ([() expr:expr]) body:expr ...+]
   #'(begin expr body ...))
  ([_ ([bindings expr:expr]) body:expr ...+]
   #'(let-values ([bindings expr]) body ...)))

(define-syntax-parser hash-expand
  ([_ prev:expr (context:id ...) ((~datum #:context) (substate:id ...) transformation:expr ...)]
   #:with (transform/context ...) (map (lambda (x) #`(hash-expand (substate ... context ...) #,x))
                                       (attribute transformation))
   #'(let ([state prev])
       (~> state
            transform/context ...)))
  ([_ prev:expr (context:id ...) ((~literal H~>) substate:id transformation:expr ...)]
   #'(let ([state prev])
       (H~> state
         ((lambda (substate)
           (H~> substate
             transformation ...)
           ) substate))))
  ([_ prev:expr (context:id ...) transform:id]
   #'(let ([state prev]) (transform state)))
  ([_ prev:expr (context:id ...) (transform:expr)]
   #'(let ([state prev]) (transform state)))
  ([_ prev:expr (context:id ...) (transform:expr (~datum *))]
   #'(let ([state prev]) (transform state) state))
  ([_ prev:expr (context:id ...) (transform:expr rw-1:kwid ...+)]
   #:with (rw-1-id ...) (filter (lambda (x) x) (attribute rw-1.id))
   #:with (rw-1-id* ...)
     (map (lambda (stx) (format-id stx "~a*" stx #:source stx))
          (attribute rw-1-id))
   #'(let* ([prev* prev]
            [rw-1-id (nested-hash-ref* prev* 'rw-1-id #f)] ...
            [context (nested-hash-ref* prev* 'context #f)] ...)
        (let-values/empty ([(rw-1-id* ...) (transform context ... rw-1 ...)])
          (~>
            prev*
            (nested-hash-set* 'rw-1-id rw-1-id*) ...)
      )))
  ([_ prev:expr (context:id ...) (transform:expr rw-1:kwid ... (reads:kwid ...) rw-2:kwid ...)]
     #:with (reads-id ...) (filter (lambda (x) x)
                                   (append (attribute rw-1.id)
                                           (attribute reads.id)
                                           (attribute rw-2.id)))
     #:with (writes-id ...) (filter (lambda (x) x)
                                    (append (attribute rw-1.id)
                                            (attribute rw-2.id)))
     #'(let* ([prev* prev]
              [reads-id (nested-hash-ref* prev* 'reads-id #f)] ...
              [context (nested-hash-ref* prev* 'context #f)] ...)
          (let-values/empty ([(writes-id ...) (transform context ... rw-1 ... reads ... rw-2 ...)])
            (~>
              prev*
              (nested-hash-set* 'writes-id writes-id) ...)))
   )
  ([_ prev:expr (context:id ...) (transform:expr rw-1:kwid ... (reads:kwid ...) rw-2:kwid ... (writes:id ...) rw-3:kwid ...)]
     #:with (reads-id ...) (filter (lambda (x) x)
                                   (append (attribute rw-1.id)
                                           (attribute reads.id)
                                           (attribute rw-2.id)
                                           (attribute rw-3.id)))
     #:with (writes-id ...) (filter (lambda (x) x)
                                    (append (attribute rw-1.id)
                                            (attribute rw-2.id)
                                            (attribute writes)
                                            (attribute rw-3.id)))
     #'(let* ([prev* prev]
              [reads-id (nested-hash-ref* prev* 'reads-id #f)] ...
              [context (nested-hash-ref* prev* 'context #f)] ...)
          (let-values/empty ([(writes-id ...) (transform context ... rw-1 ... reads ... rw-2 ... rw-3 ...)])
            (~>
              prev*
              (nested-hash-set* 'writes-id writes-id) ...)))
   )
  )

(define-syntax-parser define/H~>
  ([_ name:id terms:expr ...]
   #'(define (name state)
       (H~> state terms ...))))

(define-syntax-parser H~>
  ([_ init:expr terms:expr ...]
   #:with (term/hash-expand ...)
     (for/list ([term (in-list (attribute terms))])
       (quasisyntax/loc term (hash-expand () #,term)))
   #'(~> init term/hash-expand  ...)))

(provide top~>
         all~>
         loop~>
         variables
         )

(define-syntax-parser variables
  ([_ state:expr (name:id value:expr) ...+]
   #'(H~> state
          ((const value) () (name)) ...)))

(define-syntax-parser top~>
  ([_ name:id ...+]
   #'(lambda (state)
       (define lst (nested-hash-ref state 'name ...))
       (cond
         ([empty? lst] state)
         (else         ((first lst) state)))
       )
   ))

(define-syntax-parser top-loop~>
  ([_ name:id ...+]
   #'(lambda (state)
       (define lst (nested-hash-ref state 'name ...))
       (cond
         ([empty? lst] state)
         ([list? (first lst)]
          (define top (first lst))
          (foldl
            apply
            top
            state))
         (else         ((first lst) state)))
       )
   ))

(define-syntax-parser all~>
  ([_ name:id ...+]
   #'(lambda (state)
       (define lst (nested-hash-ref state 'name ...))
       (foldl
         (lambda (transform state)
           ((eval transform (current-namespace)) state))
         state
         lst)
       )
   ))

(define-syntax-parser loop~>
  ([_ name:id ...+]
   #'(lambda (state)
       (let loop ([state* state])
         (define lst (nested-hash-ref state* 'name ...))
         (cond
           ([empty? lst] state*)
           (else
             (define state** ((first lst) state*))
             (define lst* (nested-hash-ref state** 'name ...))
             (cond
               ([empty? lst*] state**)
               (else         (loop (nested-hash-set state** 'name ... (append (rest lst*) (list (first lst*))))))))))
       )
   ))
