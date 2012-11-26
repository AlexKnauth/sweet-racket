#lang scribble/doc

@require[scribble/manual]

@title{Sweet: an alternative to s-expressions}
@author[(author+email "Asumu Takikawa" "asumu@racket-lang.org")]

This manual describes the sweet planet package, which is a port
of the @hyperlink["http://www.dwheeler.com/readable/"]{sweet reader}
created by David Wheeler to Racket. In the port, you
can use the sweet reader as its own separate language.

To use sweet expressions, supply a #lang line like the following:

@codeblock|{
  #lang planet asumu/sweet racket

  printf("Hello")
}|

The third parameter on the #lang line is the base language to
be used with sweet expressions. You can supply any language here
such as @racket[racket], @racket[typed/racket], or others.

For example:

@codeblock|{
  #lang planet asumu/sweet typed/racket

  define: fact([n : Integer]) : Integer
    if zero?(n)
       1
       {n * fact{n - 1}}
}|

Or alternatively:

@codeblock|{
  #lang planet asumu/sweet lazy

  define fibs
    cons 0 cons(1 map(+ fibs cdr(fibs)))

  displayln list-ref(fibs 8)
}|

Known issues: quasiquotation combined with grouping behaves differently from
the original specification.