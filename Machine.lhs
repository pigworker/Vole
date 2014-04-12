> module Machine where

> import Data.Monoid

> import Stk
> import Syntax

The main entry point is `run`, which takes a stack and a closure and goes
for it. (The "monkey business" fuses an enclosing elimination with an
introduction.)

> run :: Stk Layer -> Closure -> Val
> -- regular stuff
> run stk (g, A a)      = use stk (AV a)
> run stk (g, e :& d)   = run (stk :< Car (g, d)) (g, e)
> run stk (g, vz :/ f)  = use stk (mappend g vz :/: f)
> run stk (g, V i)      = use stk (pop g i)
> run stk (g, e :$ es)  = run (stk :< Fun (g, es)) (g, e)
> run stk (g, _ := v)   = use stk v

The basic behaviour of pattern matching is to consume values and grow
environments.

> eat :: Stk Val -> Eater x -> [Val] -> (Stk Val, x)
> eat g (Burp x)    []                = (g, x)
> eat g (Lambda f)  (v : vs)          = eat (g :< v) f vs
> eat g (Ignore f)  (_ : vs)          = eat g f vs
> eat g (Split f)   ((u :&: v) : vs)  = eat g f (u : v : vs)
> eat g (Case afs)  (AV a : vs)       = let Just f = lookup a afs in eat g f vs

How to make use of a value will depend on the stack. If we're ready to
synthesize a value, we'll carry on down. Otherwise, we move right to the next
closure we need.

> use :: Stk Layer -> Val -> Val
> use S0                     v            = v                        -- home!
> use (stk :< Car c)         v            = run (stk :< Cdr v) c     -- right
> use (stk :< Cdr v)         d            = use stk (v :&: d)        -- cons
> use (stk :< Fun cs)        (g :/: h)    = eats stk g h cs          -- right
> use (stk :< Fun cs)        (AV a)       = eff stk a (S0, cs)       -- right
> use (stk :< Fun (g, [e]))  (K ls)       = run (stk <>< ls) (g, e)  -- unpack
> use (stk :< Fun (_, []))   (Y ls a vz)  = handle stk ls a vz
> use (stk :< Eat g (Han _) f cs) v
>   = eats stk g' h' cs where (g', h') = eat g f [v]
> use (stk :< Eat g (Can _) f cs) v
>   = eats stk g' h' cs where (g', h') = eat g f [(S0 :< v) :/: Return (V 0)]
> use (stk :< Eff a (vz, cs)) v = eff stk a (vz :< v, cs)
> use stk v = error $ concat ["USE ", show stk, "   ", show v]

> eff :: Stk Layer -> String -> (Stk Val, Closures) -> Val
> eff stk a (vz, (_, []))      = handle stk [] a vz
> eff stk a (vz, (g, e : es))  = run (stk :< Eff a (vz, (g, es))) (g, e)

> eats :: Stk Layer -> Stk Val -> Fun -> Closures -> Val
> eats stk g (Return e)  (_, [])       = run stk (g, e)
> eats stk g (h :? f)    (j, e : es)  = run (stk :< Eat g h f (j, es)) (j, e)
> eats stk g f cs = error $ concat
>   ["EATS ", show stk, "   ", showEnv g, "   ", show f, "   ", show cs]

Handling an effect consists of searching the stack for a function that covers
the thing invoked. We build up the continuation as we work our way out, in
the hope that the effect will garner a response that allows us to resume our
computation.

> handle :: Stk Layer -> [Layer] -> String -> Stk Val -> Val
> handle (stk :< Eat g (Han h) _ cs) ls a vz | Just j <- lookup a h =
>   let  packet S0 u = u
>        packet (vz :< v) u = packet vz (v :&: u)
>        (g', f') = eat g j [packet vz (K ls)]
>   in   eats stk g' f' cs
> handle (stk :< Eat g (Can as) f cs) ls a vz | elem a as =
>   let  (g', f') = eat g f [Y ls a vz]
>   in   eats stk g' f' cs
> handle (stk :< l) ls a vs = handle stk (l : ls) a vs
> handle S0 ls a vs = error $ concat ["HANDLE ", show ls, "   ", a, show vs]
