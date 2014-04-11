Vole
====

being a funny little programming language with a funny little abstract machine


Introduction
------------

Vole is intended as an intermediate language for other things. It's only just
getting off the ground, so there's more to come.

Vole is a bit LISP-like, but functions are black-boxed. Oh, and it's also got
some support for effects and handlers.


Concrete Syntax
---------------

*Atoms*

    A ::=  CB
    B ::=
        |  CB
        |  DB
    C ::=  ...anything but whitespace \^/.!()[]{}0123456789
    D ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

*Values*

    V ::=  A        atom
        |  []       nil
        |  [V.V]    cons cell
        |  {GF}     thunked function

    G ::=           empty environment
        |  G!V      value stored

The standard LISP convention of abbreviating `[a.[blah]]` by `[a blah]` is
observed. Whitespace is needed only to separate adjacent atoms.

*Functions*

    F ::=  .E       return expression
        |  \F       lambda (de Bruijn)
        |  \AF      lambda (named, but de Bruijn really)
        |  /F       constant function
        |  ^F       uncurry (act on cons-cells by acting on car then cdr)
        |  (S)      switch on an atom
        |  [S]F     handle a permitted effect

    S ::=           no choice
        |  AF S     at least one choice

*Expressions*

    E ::=  A        atom (or named bound variable)
        |  []       nil
        |  [E.E]    cons operation
        |  {GF}     thunked function
        |  !A       definition
        |  DI       de Bruijn index
        |  (EQ)     application

    Q ::=           no arguments
        |   EQ      an argument then maybe some more

    I ::=           no more digits
        | DI        a digit, then maybe some more

*Programs*

    P ::=           no more definitions
        | !AV P     a definition, then maybe more

Parsing is not a big ask, except for supporting named lambdas. Note that each
variable bound with a name is also de Bruijn indexed, so `\x\y.x` can be written
`\x\y.1` or just `\\.1`. The names don't survive the parser, so the semantics is
de Bruijn.

*Examples*

If we want to compute with a list datatype, we need to give it a tagged format,
as Vole does not let you test for being an atom, only to distinguish cases for
values *known* to be an atom and to project from values *known* to be pairs.

    !abc   [cons a [cons b [cons c [nil]]]]
    !map   {\f^(nil/.[nil] cons^\x^\xs/.[cons (f x) (!map f xs)])}
    !wrap  {\x.[wrap x]}

With these definitions, evaluating

    (!map !wrap !abc)

should yield

    [cons [wrap a] [cons [wrap b] [cons [wrap c] [nil]]]]


Operational Semantics
---------------------

Vole can be run on a stack-and-closure machine, so we haven't left Landin-land.
But you get to be quite fly with stacks.

*Closures*

    R ::=  G.E

Before we get there, let's look at how pattern matching is the process of computing
a closure from a function and a sequence of values

    g .e                                    is the accepting state
    g \f         v vs  --->  g!v f vs       lambda abstracts
    g /f         v vs  --->    g f vs       constant ignores
    g ^f [u.v]   vs    --->    g f u v vs   uncurry splits
    g (..a f..)  a vs  --->    g f vs       switching selects
    g [..]f      vs    --->    g f vs       no effect to handle

For example

                      ^(nil/.[nil] cons^\^\/.0) [cons a [cons b [nil]]]  --->
                      (nil/.[nil] cons^\^\/.0) cons [a [cons b [nil]]]   --->
                      ^\^\/.0 [a [cons b [nil]]]                         --->
                      \^\/.0 a [[cons b [nil]]                           --->
                   !a ^\/.0 [[cons b [nil]]                              --->
                   !a \/.0 [cons b [nil] []                              --->
    !a![cons b [nil]] /.0 []                                             --->
    !a![cons b [nil]] .0

*Stacks*

    K ::=             top level
        |  KL         in the middle of doing something

    L ::=  [-.R]      computing a car, with cdr to come
        |  [V.-]      computing a cdr, with car already done
        |  (-U)       computing a function, with arguments to come
        |  (VG-U)     computing an argument, function and some arguments done,
                        but other arguments to come

    U ::=             no more arguments
        |   RU        an argument closure pending computation

