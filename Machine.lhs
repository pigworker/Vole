> module Machine where

> import Data.Monoid

> import Stk
> import Syntax

The main entry point is `run`, which takes a stack and a closure and goes
for it. (The "monkey business" fuses an enclosing elimination with an
introduction.)

> run :: Stk Layer -> Closure -> Val
> -- monkey business
> run (stk :< Args (h :/: Switch afs) (S0, cs)) (g, A a)
>   = let Just f = lookup a afs in args stk (h :/: f) (S0, cs)
> run (stk :< Args (h :/: Split f) (S0, cs)) (g, e :& d)
>   = run (stk :< Args (h :/: f) (S0, (g, d) : cs)) (g, e)
> -- regular stuff
> run stk (g, A a)      = use stk (AV a)
> run stk (g, e :& d)   = run (stk :< Car (g, d)) (g, e)
> run stk (g, vz :/ f)  = use stk (mappend g vz :/: f)
> run stk (g, V i)      = use stk (pop g i)
> run stk (g, e :$ es)  = run (stk :< Fun (map ((,) g) es)) (g, e)
> run stk (g, _ := v)   = use stk v


How to make use of a value will depend on the stack. If we're ready to
synthesize a value, we'll carry on down. Otherwise, we move right to the next
closure we need.

> use :: Stk Layer -> Val -> Val
> -- monkey business
> use (stk :< Args (g :/: Lambda f) (S0, cs)) v
>   = args stk ((g :< v) :/: f) (S0, cs)
> use (stk :< Args (g :/: Ignore f) (S0, cs)) v
>   = args stk (g :/: f) (S0, cs)
> -- regular stuff
> use S0                        v       = v                         -- home!
> use (stk :< Car c)            v       = run (stk :< Cdr v) c      -- right
> use (stk :< Cdr v)            d       = use stk (v :&: d)         -- cons
> use (stk :< Fun [c])          (K ls)  = run (stk <>< ls) c        -- unpack
> use (stk :< Fun cs)           f       = args stk f (S0, cs)       -- right
> use (stk :< Args f (vz, cs))  v       = args stk f (vz :< v, cs)  -- right

The smart constructor for `Args` looks to see if we've just finished the
last argument. If not, go right. If so, run the code or handle the effect!

> args :: Stk Layer -> Val -> (Stk Val, [Closure]) -> Val
> args stk v          (vz, (c : cs))  = run (stk :< Args v (vz, cs)) c
> args stk (g :/: f)  (vz, [])        = munch stk g f (vz <>> [])
> args stk (AV a)     (vz, [])        = handle stk [] a (vz <>> [])

Running a function starts by processing its arguments, left-to-right. As you
can see, it's on the programmer to ensure that `Split` splits and `Switch`
switches.

> munch :: Stk Layer -> Stk Val -> Fun -> [Val] -> Val
> munch stk g (Return e)    []                = run stk (g, e)
> munch stk g (Lambda f)    (v : vs)          = munch stk (g :< v) f vs
> munch stk g (Ignore f)    (_ : vs)          = munch stk g f vs
> munch stk g (Split f)     ((u :&: v) : vs)  = munch stk g f (u : v : vs)
> munch stk g (Switch afs)  (AV a : vs)       = munch stk g f vs
>   where Just f = lookup a afs
> munch stk g (Handle afs f) vs               = munch stk g f vs
> munch stk g f vs = error $ concat
>   ["MUNCH ", show stk, "   ", show g, "   ", show f, "   ", show vs]

Handling an effect consists of searching the stack for a function that covers
the thing invoked. We build up the continuation as we work our way out, in
the hope that the effect will garner a response that allows us to resume our
computation.

> handle :: Stk Layer -> [Layer] -> String -> [Val] -> Val
> handle (lz :< Args (g :/: Handle afs _) (vz, cs)) ls a vs
>   | Just f <- lookup a afs
>   = args lz (g :/: f) (vz <>< vs :< K ls, cs)
> handle (lz :< l) ls a vs = handle lz (l : ls) a vs
> handle S0 ls a vs = error $ concat ["HANDLE ", show ls, "   ", a, show vs]
