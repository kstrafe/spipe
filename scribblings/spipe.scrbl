#lang scribble/manual
@require[@for-label[spipe
                    racket/base]]

@require[scribble/example]

@title{spipe}
@author{Kevin R. Stravers}

@defmodule[spipe]

Spipe is a syntax transformer that takes care of hash-table accesses and updates. Spipe threads state (a hash-table) from top to bottom using the provided @italic{transformations}. Each @italic{transformation} specifies which element will be read and written to in the hash-table.

@defform[(spipe table-expr transformer ...)
  #:grammar [(transformation (procedure transform ...))
             (transform (code:line) r:accessor w:accessor rw:accessor (r: accessor ...+) (w: accessor ...+) (rw: accessor ...+))
             (accessor (code:line) id id.accessor)
             ]
  #:contracts ([table-expr hash?]
               [procedure procedure?])]{
  Each accessor can be provided in dot-notation (a.b.c and so on), where each dot denotes a nested hash table. If an entry does not exist for reads, @racket[#f] is sent to the function, but no entry is made in the hash-table.

  Reads and writes are order-sensitive. Reads (anything with r:) are sent to the @racket[procedure] in order. Any keywords between the reads also follows that order. The writes bind to the returned @racket[(values)] in the order the writes are specified.

  Accessors without an r: or w: tag are automatically rw:.

  It's also possible to not specify any write arguments in which case the hash-table will simply pass through the function. This may be useful when side-effects are desired. Not having any r: accessors is possible too, allowing one to directly assign values to the hash-table.

  Note that procedure can also be a syntax transformer but this is rather unconventional. Only the read and keyword arguments will be seen by this transformation, which are provided in their original syntax form with accessor tags removed.
}

@section{Examples}

@subsection{Basic Examples}

A transformation begins by first specifying a hash-table and then listing the subsequent operations.
@examples[
(require spipe)
(spipe
  (make-immutable-hash '((value . 0)))
  (add1 value))
]

In the above @racket[spipe] works by applying @racket[add1] to the entry @racket['value] inside the hash-table. The first argument to spipe is always a hash-table - not seen as a transformation - and may be any arbitrary expression.

@racket[value] is untagged and is thus considered rw:, meaning that it is used both as input to @racket[add1] as well as output from @racket[add1]. If this is undesirable, one can manually tag inputs and outputs.

@examples[
(require spipe)
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
(require spipe)
(spipe
  (make-immutable-hash '((a . 1) (b . 2) (c . 3)))
  (+  (r: a b c) w:result.positive)
  (-  r:a r:b r:c w:result.negative))
]

When one has multiple inputs/outputs, it's desired to avoid duplicating the r:, w:, and rw: tags. One ought to use (r: ...) and so on instead.

@subsection{Nested Access}

@examples[
#:label "Example of nested accessors and defaults for non-existent elements."
(require spipe)
(spipe
  (hash)
  ((lambda (x) (if x 1 0))  r:does.not.exist  w:the.result))
]

Reads of non-existent elements default to @racket[#f], while writes simply insert the value if the entry did not already exist in the hash-table. Otherwise writes overwrite the previous value.

@subsection{Keywords}

@examples[
#:label "Keywords are order-sensitive"
(require spipe)

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
(require spipe)
(spipe
  (hash)
  ((lambda () (values 1 2 3))  (w: a b c)))
]

@subsection{Side-effects}

@racket[spipe] has no side-effects, but one can use procedures which produce side-effects as transformations. If no writes are intended, one can leave out all w: and rw: accessors.

@examples[
#:label "Using side-effects"
(require spipe)

(spipe
  (make-immutable-hash '((value . "test value")))
  (writeln r:value))
]

@subsection{Usage With Macros}

Macros can be used for procedures. Spipe guarantees that the read arguments are provided to the given syntax transformer. 

@examples[
(require spipe syntax/parse/define (for-syntax racket/base))
(define-syntax-parser print-both
  ([_ x] #'(writeln `(x ,x))))
(spipe
  (make-immutable-hash '((value . 123)))
  (print-both r:value))
]

@subsection{Entire Table}

Sometimes we just want to send the entire table to some function. To do so we specify no inputs and no outputs.

@examples[
(require spipe)
(spipe
  (make-immutable-hash '((value . 123)))
  (writeln))
]

Returned values are ignored.
