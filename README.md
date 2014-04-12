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
        |  {GH}     thunked handler

    G ::=           empty environment
        |  G!V      value stored

The standard LISP convention of abbreviating `[a.[blah]]` by `[a blah]` is
observed. Whitespace is needed only to separate adjacent atoms.

*Expressions*

    E ::=  A        atom (or named bound variable)
        |  []       nil
        |  [E.E]    cons operation
        |  {GH}     thunked handler
        |  !A       definition
        |  DI       de Bruijn index
        |  (EQ)     application

    Q ::=           no arguments
        |   EQ      an argument then maybe some more

    I ::=           no more digits
        | DI        a digit, then maybe some more

*Handlers and Functions*

A handler might offer to intercept effectful operations requested by an argument,
and it will certainly say how to make functional use of any value delivered by the
argument.

    H ::=  .E       no more arguments, deliver return value
        |  [S]F     a choice of effect handlers, a function on returned values
        |  F        as above, but with the empty choice of handlers
        |  {Y}F     a set of effects to trap and suspend, a function on the thunk

    F ::=  H        handle next thing
        |  \F       lambda (de Bruijn)
        |  \AF      lambda (named, but de Bruijn really)
        |  /F       constant function
        |  ^F       uncurry (act on cons-cells by acting on car then cdr)
        |  (S)      switch on an atom

    S ::=           no choice
        |  AF S     at least one choice

    Y ::=           no atoms
        |  AY       one or more atoms

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

    R ::=  (G.E)

*Stacks*

    K ::=                top level
        |  KL            in the middle of doing something

    L ::=  [-.R]         computing a car, with cdr to come
        |  [V.-]         computing a cdr, with car already done
        |  (-GQ)         computing a function, with arguments to come
        |  (GH-G.Q)      computing an argument, function and some arguments
                         done, but other arguments to come
        |  (AG-G.Q)      computing an argument to an effect, as above

There are some more, but these are the main things.

*Running*

We run an expression by pushing layers onto the stack until we find a value
we can use.

    RUN k (g.a)       ---> USE k a
    RUN k (g.{g'h})   ---> USE k {g'h}
    RUN k (g.!a)      ---> USE k <defined value of a>
    RUN k (g!v.0)     ---> USE k v
    RUN k (g!v.i+1)   ---> RUN k (g.i)
    RUN k (g.[e.e'])  ---> RUN k[-.(g.e')] (g.e)
    RUN k (g.(e q))   ---> RUN k(-g.q) (g.e)
    
*Using*

This is where the real work happpens.

    USE          v         ---> v
    USE k[-.r]   v         ---> RUN k[v.-] r
    USE k(-g.)   {g'.e}    ---> RUN k (g'.e)
    USE k(-g.eq) {g'h}     ---> RUN k(g'h-g.q) (g.e)
    USE k(g'[s]f-g.) v     ---> RUN k (g''.e) if EAT g' f v ---> g'' .e
    USE k(g'[s]f-g.eq) v   ---> RUN k(g''h-g.q) (g.e) if EAT g' f v ---> g'' h

Application works by feeding the argument values to the handler one at a time.
That is, the function's matching process is *coroutined* with the argument
execution processes. Here's what the function does with ordinary values.

*Eating*

A function eats a value sequence according to the following rules. We always
start with one value and end with none.

    EAT g h                --->  g h
    EAT g \f         v vs  --->  EAT g!v f vs       lambda abstracts
    EAT g /f         v vs  --->  EAT   g f vs       constant ignores
    EAT g ^f [u.v]   vs    --->  EAT   g f u v vs   uncurry splits
    EAT g (..a f..)  a vs  --->  EAT   g f vs       switching selects

Once the value is fully consumed, we find the environment extended and the
handler for the rest of the arguments.

For example,

    EAT    ^(nil/.[nil] cons^\^\/.0)   [cons a [cons b [nil]]]  --->
    EAT    (nil/.[nil] cons^\^\/.0)    cons [a [cons b [nil]]]  --->
    EAT    ^\^\/.0                     [a [cons b [nil]]]       --->
    EAT    \^\/.0                      a [[cons b [nil]]        --->
    EAT !a   ^\/.0                     [[cons b [nil]]          --->
    EAT !a   \/.0                      [cons b [nil] []         --->
    EAT !a![cons b [nil]]   /.0   []                            --->
    !a![cons b [nil]] .0

*Invoking Effects*

Using a naked atom in a function position means calling the *effect* with that
symbol. (Calling a defined function is done with a bang!)

    USE k(-g.)       a  ---> HANDLE k a
    USE k(-g.eq)     a  ---> RUN k(a-g.q) (g.e)
    USE k(ag'-g.)    v  ---> HANDLE k a g'!v
    USE k(ag'-g.eq)  v  ---> RUN k(qg'!v-g.q) (g.e)

Once we know we're going to be calling an effect, we evaluate and pile up its
arguments, and when they've all arrived, we handle them, taking a march down the
stack in search of somehthing that will deal with them.

*Handling Effects*

I, er, forgot to tell you that a forward sequence of stack layers is
also a function of arity 1, called a *continuation* (or would
*resumption* be better?. You apply it by repushing it on the stack
*before* evaluating the argument. Effect handling relies on
continuations.

  * find a handling function `f` for the given effect atom `a` on the stack, in some
      `(g'[..a f..]f'-g.q)` layer accumulating the continuation of stack layers above it
  * construct the effect packet by consing the environment of effect arguments
      onto the continuation
  * use the handling function `f` to eat the effect packet, then use the resulting
      handler to consume the pending closures `g.q` in the resulting environment

