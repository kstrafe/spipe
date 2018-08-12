#lang scribble/manual
@require[@for-label[spipe
                    racket/base
                    racket/function]]

@require[racket/sandbox scribble/example]

@(define evaluator (parameterize ([sandbox-output 'string]
                                  [sandbox-error-output 'string]
                                  [sandbox-memory-limit 500])
                     (make-evaluator 'racket '(require spipe))))

@title[#:tag "spipe"]{Super Pipe}
@author{Kevin R. Stravers}

@defmodule[spipe]

The @racket[H~>] form implements @racket[hash]-based pipeline programming. @racket[H~>] functionally threads state (a hash-table) through provided @italic{transformation}s. Each @italic{transformation} specifies which elements to access and update in the hash table.

@defform[(H~> table transformation ...)
         #:contracts ([table hash?])]{
  Uses the state @racket[table] and applies the @racket[transformation]s to it from left-to-right. Each @racket[transformation] may introduce new entries in the state, which will be visible for the next @racket[transformation]. @racket[table] is an arbitrary expression resulting in @racket[hash?].

  The grammar of @racket[transformtion] follows, where non-italicized identifiers are recognized symbolically.

  @racketgrammar*[
    #:literals (*)
    [transformation 
      (code:line callee-id)
      (code:line (callee))
      (code:line (callee *))
      (code:line (callee read-writes ...+))
      (code:line (callee (reads ...) (writes ...)))
      (code:line (callee (reads ...)))
      ]
  ]

  The first transformation form @racket[callee-id] is always transformed into @racket[(callee)].

  @racket[(callee)] applies the form with the entire state as its argument, and ignoring all return values.

  @racket[(callee *)] is a special form that is similar to @racket[(callee)] but it replaces the state with @racket[callee]'s return value.

  @racket[(callee read-writes ...+)] simultaneously specifies variables to read from and write to. The reads are given as arguments to @racket[callee] in the order they are specified. The writes are assigned to the returned @racket[value]s in the order they are specified.

  @racket[(callee (reads ...) (writes ...))] specifies reads and writes separately. Left-to-right order applies.

  @racket[(callee (reads ...))] for when you intend to not write to any entry.

  @examples[#:eval evaluator #:label "Simple example of usage"
    (H~>
      (hash 'hello "to you " 'world 2)
      (number->string world)
      (string-append (hello world) (hw)))
  ]

  Note that the above adds the symbol @racket['hw] to the hash-table. If a write location does not exist, it is created. Entries in the hash table can not be deleted inside @racket[H~>] without using external functions.

  @examples[#:eval evaluator #:label "Example of all forms:"
    (H~>
      (hash 'hello "hi" 'world "u")
      write
      (print          )
      (identity      *)
      ((const "you") world)
      (string-append (hello world) (hello-world))
      (displayln     (hello-world)))
  ]
@section{Nested Attributes}

Sometimes it's useful to access nested hash tables. @racket[H~>] provides functionality for this by parsing any identifier containing dots. Each dot represents a sub-table entry.

  @examples[#:eval evaluator #:label "Example of nested attributes:"
    (H~>
      (hash)
      ((const 'value) () (a.b.c.target))
      (write (a.b))
      )
  ]

For writes, if a subtable does not exist, it is created. However, if such creation would overwrite already-existing values, an error is thrown.
For reads, non-existing subtables return @racket[#f].
}
