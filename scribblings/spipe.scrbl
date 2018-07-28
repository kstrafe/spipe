#lang scribble/manual
@require[@for-label[spipe
                    racket/base]]

@require[racket/sandbox scribble/example]

@(define evaluator (parameterize ([sandbox-output 'string]
                                  [sandbox-error-output 'string]
                                  [sandbox-memory-limit 500])
                     (make-evaluator 'racket '(require spipe))))

@title[#:tag "spipe"]{Super Pipe}
@author{Kevin R. Stravers}

@defmodule[spipe]

The @racket[spipe] form implements @racket[hash]-based pipeline programming. @racket[spipe] functionally threads state (a hash-table) through provided @italic{transformation}s. Each @italic{transformation} specifies which elements to access and update in the hash table.

@defform/subs[(spipe hash transformation ...)
              ([transformation (call transform ...)])
              #:contracts ([hash hash?])]{
  Uses the state @racket[hash] and applies the @racket[transformation]s to it from left-to-right. Each @racket[transformation] may introduce new entries in the state, which will be visible for the next @racket[transformation]. @racket[hash] is an arbitrary expression resulting in @racket[hash?].

  The grammar of @racket[transform] follows, where non-italicized identifiers are recognized symbolically.


@racketgrammar*[
  #:literals (r: w: rw: e: x:)
  [transform (code:line read)
             write
             read-write
             external
             expression

             multi-read
             multi-write
             multi-read-write
             multi-external
             multi-expression]
  [read (code:line r:accessor) (code:line keyword r:accessor)]
  [write w:accessor]
  [read-write (code:line accessor) (code:line rw:accessor) (code:line keyword rw:accessor)]
  [external (code:line e:accessor) (code:line keyword e:accessor)]
  [expression (code:line x: expr) (code:line keyword x: expr)]

  [multi-read (code:line (r: tagless ...+)) (code:line keyword (r: id tagless ...))]
  [multi-write (code:line (w: id ...+))]
  [multi-read-write (code:line (rw: tagless ...+)) (code:line keyword (rw: id tagless ...))]
  [multi-external (code:line (e: tagless ...+)) (code:line keyword (e: id tagless ...))]
  [multi-expression (code:line (x: kw-exprs ...+)) (code:line keyword (x: id kw-exprs ...))]

  [tagless (code:line accessor) (code:line keyword accessor)]
  [kw-exprs (code:line expr) (code:line keyword expr)]
  [accessor (code:line id) id.accessor]
]

More specifically, the following rules apply:

@itemize[

         @item{@racket[r:accessor] as well as their @racket[w:], @racket[rw:], @racket[e:], and @racket[x:] counterparts are intended to be prepended to the identifier in order to match.
            @examples[#:eval evaluator
              (spipe (hash) (identity r:test-read w:test-write))
            ]
            @racket[spipe] looks for these prefixes to determine how to order arguments and store values inside the hash-table.
         }

         @item{@racket[accessor] is any dot-separated identifier
            @examples[#:eval evaluator
              (spipe (hash) (identity r:a.b.c w:d.e.f))
            ]
            Each dot denotes a child hash-table. If a write accesses a non-existent variable then a hash-table will
            be created, but if the variable already exists and is not @racket[hash?] then an exception is thrown.
            @examples[#:eval evaluator
              (eval:error (spipe (hash) ((const 123) w:a) (identity x: 1 w:a.b)))
            ]
         }

         @item{An @racket[id] in place of transform implies rw:
            @examples[#:eval evaluator
              (spipe (hash) ((const 1) variable))
            ]
         }

         @item{If there are multiple similar accesses (i.e.: @racket[r:a r:b ...]) then one can group these under @racket[(r:)]:
            @examples[#:eval evaluator
              (spipe (hash) (values (x: 1 2 3) (w: a b c)))
            ]
         }
      ]

  Reads and writes are order-sensitive. Reads (anything with r: rw: e: and x:) are sent to the @racket[call] in the order they are specified. Any keywords also follow that order. The writes bind to the returned values in the order the writes are specified.

  It's also possible to not specify any arguments in which case the hash-table will simply pass through the function. This may be useful when side-effects are desired. In this case the return value of @racket[call] is ignored. Only having w: accessors is possible too, allowing one to directly assign values to the hash-table.

  Note that @racket[call] can also be a syntax transformer. Only the read and keyword arguments will be seen by this transformation, which are provided in their original syntax form with accessor tags removed but with the same context and source location. Using a syntax transformer without any arguments sends it a single argument called @italic{table}, containing the entire state.
}}

@section{Examples}

@subsection{Basic Examples}

A transformation begins by first specifying a hash-table and then listing the subsequent operations.
@examples[#:eval evaluator
(spipe
  (make-immutable-hash '((value . 0)))
  (add1 value))
]

In the above @racket[spipe] works by applying @racket[add1] to the entry @racket['value] inside the hash-table. The first argument to spipe is always a hash-table - not a transformation - and is an arbitrary expression as that yields @racket[hash?].

@racket[value] is untagged and is thus considered rw:, meaning that it is used both as input to @racket[add1] as well as output from @racket[add1]. If this is undesirable, one can manually tag inputs and outputs.

@examples[
#:eval evaluator
(spipe
  (make-immutable-hash '((value . 0)))
  (add1 r:value w:result))
]

@examples[
#:label "More complex example using multiple transformations"
(require racket/function racket/math spipe)
(spipe
  (hash)
  ((const 10)  w:x)
  (add1        rw:x)
  (sqr       rw:x)
  (identity  r:x w:y))
]

One can substitute any algorithm for the above example.

@examples[
#:label "Multiple ways of writing inputs"
#:eval evaluator
(spipe
  (make-immutable-hash '((a . 1) (b . 2) (c . 3)))
  (+  (r: a b c) w:result.positive)
  (-  r:a r:b r:c w:result.negative))
]

When one has multiple inputs/outputs, it's desired to avoid duplicating the r:, w:, and rw: tags. One ought to use (r: ...) and so on instead.

@subsection{Nested Access}

@examples[
#:label "Example of nested accessors and defaults for non-existent elements."
#:eval evaluator
(spipe
  (hash)
  ((lambda (x) (if x 1 0))  r:does.not.exist  w:the.result))
]

Reads of non-existent elements default to @racket[#f], while writes simply insert the value if the entry did not already exist in the hash-table. Otherwise writes overwrite the previous value.

@subsection{Keywords}

@examples[
#:label "Keywords are order-sensitive"
#:eval evaluator

(define (example a #:extra [b 10]) (+ (/ a 2) b))

(spipe
  (make-immutable-hash '((x . 2) (y . 3)))
  (example r:x #:extra r:y w:result-1)
  (example #:extra r:x r:y w:result-2))
]

The first read tagged identifier after a keyword is the value used for that keyword-argument to the function. Multiple keyword-arguments are possible.

@margin-note{Keywords also work with the (r: ...) format, whether the keyword is inside or outside the parentheses.}

@subsection{Multiple Return Values}

@examples[
#:label "Multiple return values"
#:eval evaluator
(spipe
  (hash)
  ((lambda () (values 1 2 3))  (w: a b c)))
]

@subsection{Side-effects}

@racket[spipe] has no side-effects, but one can use procedures which produce side-effects as transformations. If no writes are intended, one can leave out all w: and rw: accessors and the state will simply pass through the transformation.

@examples[
#:label "Using side-effects"
#:eval evaluator

(spipe
  (make-immutable-hash '((value . "test value")))
  (writeln r:value))
]

@subsection{Usage With Macros}

Macros can be used for procedures. Spipe guarantees that the read arguments are provided to the given syntax transformer.

@examples[
#:eval evaluator
(require syntax/parse/define (for-syntax racket/base))
(define-syntax-parser print-both
  ([_ x] #'(writeln `(x ,x))))
(spipe
  (make-immutable-hash '((value . 123)))
  (print-both r:value))
]

@subsection{Entire Table}

Sometimes we just want to send the entire table to some function. To do so we specify no inputs and no outputs.

@examples[
#:eval evaluator
(spipe
  (make-immutable-hash '((value . 123)))
  (writeln))
]

Returned values are ignored.

@subsection[#:tag "ext-acc"]{External Accessor}

It may not be practical to put some values inside the hash table, nor is it comfortable to curry, in such a case you may want to use the e: access specifier.

@examples[
#:eval evaluator
(define my-value 100)
(spipe
  (hash)
  (add1 e:my-value w:something.else))
]

@margin-note{e: functions just like r:, but instead of reading from the hash we read from the environment.}

e: does not split on . or do any processing. It directly references variables in the environment.

@subsection{Inline Expressions}

Expressions use the x: tag.

@examples[
#:eval evaluator
(spipe (hash)
       (identity x: (+ 1 2 3) w:result))
]

Expressions are applied to the function following the r: order. Keywords apply all the same as any read or external reference does.

@examples[
#:eval evaluator
(spipe (hash)
       (- (x: (+ 1 2 3) 10) w:result))
]

Multiple expressions can be grouped by creating a list starting with x:.
