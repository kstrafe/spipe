#lang racket

(provide access)

(require (for-syntax racket/function racket/list racket/pretty racket/string racket/syntax)
         (for-meta 2 racket/base)
         syntax/parse/define)

(begin-for-syntax
  (define (unify . lst) (filter-not empty? (filter identity (flatten lst))))
  (define-splicing-syntax-class maybe-kw (pattern (~optional kw:keyword)))
  (define-splicing-syntax-class term
    (pattern (~or
               (~seq #:w                            w:expr  ...+)
               (~and
                 (~or
                   (~seq #:r   (~and
                                 (~seq                read  ...+)
                                 (~seq kw:maybe-kw  r:expr  ...+)))
                   (~seq #:rw  (~and
                                 (~seq          read/write  ...+)
                                 (~seq kw:maybe-kw rw:expr  ...+)))
                   (~seq #:x   (~and
                                 (~seq         expressions  ...+)
                                 (~seq kw:maybe-kw  x:expr  ...+)))
                   ))
               )
             #:attr (read*  1) (unify (attr read)
                                      (attr read/write))
             #:attr (write* 1) (unify (attr w)
                                      (attr rw))
             ))

  (define (expand-accessors transformer lst n)
    (map (lambda (x)
           (if x
             (map
               (lambda (x)
                 (local-expand #`(#,transformer #,x #,@(flatten n)) 'expression #f))
               x)
             empty))
         lst))
  (define-syntax attr (make-rename-transformer #'attribute)))

(define-syntax-parser get* ([_ kw:keyword] #'kw) ([_ n:id]   #'n))
(define-syntax-parser set* ([_ i:id n ...]       #'i))
(define-syntax-parser xet* ([_ kw:keyword a ...] #'kw) ([_ e:expr a ...] #'e))

(require syntax/parse/define (for-syntax racket/list))
;; A permissive let*-values that allows ignore bindings
(define-syntax-parser let*-values*
  ([_ () f:expr ...+] #'(begin f ...))
  ([_ ([() e:expr] rest:expr ...) f:expr ...+]
   #'(begin e (let*-values* (rest ...) f ...)))
  ([_ ([(n:id ...+) e:expr] rest:expr ...) f:expr ...+]
    #'(let*-values ([(n ...) e])
        (let*-values* (rest ...)
          f ...)))
  )

;; What does this do to justify its existence?
;; 1. Calls a function using all #:r, #:rw, and #:x inputs
;; 2. Stores results in #:w inputs
;; 3. Macro-transforms all #:r, #:w, (including #:rw), and #:x
;; 4. Allows arbitrary macros to transform the meaning
(define-syntax-parser access
  ([_ (~or
        (~optional (~seq #:get getter) #:defaults ([getter #'get*])
                   #:too-many "#:get has already been specified")
        (~optional (~seq #:set setter) #:defaults ([setter #'set*])
                   #:too-many "#:set has already been specified")
        (~optional (~seq #:xet xetter) #:defaults ([xetter #'xet*])
                   #:too-many "#:xet has already been specified")
        (~optional (~seq #:surround (~and (~seq surround ...)
                                          (~seq [(n:id ...) f:expr] ...)))
                   #:defaults ([(n 2)        empty]
                               [(surround 1) empty])
                   #:too-many "#:surround has already been specified")
        (~optional (~seq #:result result) #:defaults ([result #'values])
                   #:too-many "#:result has already been specified")
        ) ...
      accessor:term ...]
   #:with ((r ...)  ...) (expand-accessors (attr getter) (attr accessor.read*)       (attr n))
   #:with ((w ...)  ...) (expand-accessors (attr setter) (attr accessor.write*)      (attr n))
   #:with ((x ...)  ...) (expand-accessors (attr xetter) (attr accessor.expressions) (attr n))
   #:with ((rx ...) ...) (map append (attr r) (attr x))
   (parameterize ([print-syntax-width +inf.0]) (void))
   #'(let*-values* (surround ...
                    [(accessor.write* ... ...) (rx ... ...)])
      (result n ... ... w ... ...))
   ))

(module+ test
  (require rackunit)
  (check-equal? 
    (access #:x identity (+ 1 2) #:w result)
    3)
  (check-equal? 
    (access #:x identity (+ 1 2) #:w result)
    3)
  (check-equal? 
    (access #:x + (+ 1 2) 3 #:w result)
    6)
  )
