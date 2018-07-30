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
             #:attr (read*  1) (unify (attribute read)
                                      (attribute read/write))
             #:attr (write* 1) (unify (attribute w)
                                      (attribute rw))
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


;; (access #:get r #:set w #:xet x #:surround [(n) b]
;;   #:result res
;;   #:x f #:r a b c #:w d e f #:rw g h i #:x (+ 1 2))
;; ->
;; (let-values ([(n) b]
;;              [(d e f) ((x f n) (r a n) (r b n) (r c n) (r g n) (r h n) (r i n) (x (+ 1 2) n))])
;;   (res n (w d) (w e) (w f))

;; A let*-values that puts empty bindings into a void binding

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
; (let*-values* ([() (writeln 'mia-amigo)]
;                [(a) (+ 1 2)]
;                [() (writeln 'test)]
;                [(b) (- 1 2)])
;               (values a b))

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
   (parameterize ([print-syntax-width +inf.0])
     ; (pretty-print (attr r))
     ; (pretty-print (attr w))
     ; (pretty-print (attr x))
     ; (pretty-print `(n: ,(attr n)))
     ; (pretty-print `(f: ,(attr f)))
     ; (pretty-print `(s: ,(attr surround)))
     ; (pretty-print `(acc.write* ,(attribute accessor.write*)))
     ; (pretty-print `(acc.write*+ ,(not (empty? (flatten (attribute accessor.write*))))))
     (void)
     )
   #'(let*-values* (surround ...
                    [(accessor.write* ... ...) (rx ... ...)])
      (result n ... ... w ... ...))
   ;; THIS WORKED
   ; (cond
   ;   ([attribute surround]
   ;    (writeln 'first)
   ;    (writeln `(surround ,(attribute surround)))
   ;    (writeln `(acc.write ,(attribute accessor.write*)))
   ;    (if (empty? (flatten (attribute accessor.write*)))
   ;      #`(let*-values (surround ...)
   ;                     (rx ... ...)
   ;                     (result n ... ...))
   ;      #`(let*-values (surround ... [(accessor.write* ... ...) (rx ... ...)])
   ;                     (result n ... ... w ... ...))))
   ;   ([not (empty? (flatten (attribute accessor.write*)))]
   ;    (writeln 'second)
   ;    #`(let-values  ([(accessor.write* ... ...) (rx ... ...)])
   ;                   (result w ... ...)))
   ;   (else
   ;    (writeln 'third)
   ;    #'(rx ... ...))
   ;                 )
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
