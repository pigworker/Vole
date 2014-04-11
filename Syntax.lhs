> module Syntax where

> import Data.List

> import Stk

The first-order data values are atoms or cons-cells, but unlike LISP, the
higher-order values are black-boxed: they are closures or
continuations.

> data Val
>   = AV String          -- atom
>   | Val :&: Val        -- cons
>   | Stk Val :/: Fun    -- closure
>   | K [Layer]          -- continuation

Expressions have the same stuff as values (apart from continuations, which
should be generated only by the machine. We also have variables (de Bruijn
indices), application, and appeal to explicit cached definition.

> data Exp
>   = A String
>   | Exp :& Exp
>   | Stk Val :/ Fun
>   | V Int
>   | Exp :$ [Exp]
>   | String := Val

A function processes a list of arguments, growing an environment, the while.

> data Fun
>   = Return Exp                    -- arguments done, so compute value
>   | Lambda Fun                    -- push first arg onto environment
>   | Ignore Fun                    -- drop first arg
>   | Split Fun                     -- replace first arg by its car and cdr
>   | Switch [(String, Fun)]        -- remove and branch on atomic first arg
>   | Handle [(String, Fun)] Fun    -- be prepared

There are some invariants not managed here:
  * you have to know when to `Split` and when to `Switch`
  * you have to mind the arity of the function yourself
  * you should `Handle` only in *parent* positions, not when processing
      the children of a `Split`

The stack layers reflect what we might be in the middle of. It's a *dissection*.

> type Closure = (Stk Val, Exp)
> data Layer
>   = Car Closure                    -- evaluating a car, with cdr pending
>   | Cdr Val                        -- evaluating a cdr, with car parked
>   | Fun [Closure]                  -- awaiting a function, arguments pending
>   | Args Val (Stk Val, [Closure])  -- awaiting the value of an argument
>   deriving Show

Invariant: in an `Args` layer, the function (the lone `Val`) should be
either a closure (ready to compute) or an atom (signalling an effect
we're about to invoke). It should never be a cons-cell or a continuation:
the latter should be unpacked and fished onto the stack directly, so that
its handlers are clearly visible.

Boring `Show` implementations.

> instance Show Val where
>   show (AV "") = "[]"
>   show (AV a) = a
>   show (v :&: d) = "[" ++ show v ++ cdr d where
>     cdr (AV "") = "]"
>     cdr (v :&: d) = " " ++ show v ++ cdr d
>     cdr v = "," ++ show v ++ "]"
>   show (vz :/: f) = "{" ++ showEnv vz ++ show f ++"}" where
>   show (K ls) = show ls

> showEnv :: Stk Val -> String
> showEnv S0 = ""
> showEnv (vz :< v) = showEnv vz ++ "!" ++ show v

> instance Show Exp where
>   show (A "") = "[]"
>   show (A a) = a
>   show (e :& d) = "[" ++ show e ++ cdr d where
>     cdr (A "") = "]"
>     cdr (e :& d) = " " ++ show e ++ cdr d
>     cdr e = "," ++ show e ++ "]"
>   show (vz :/ f) = "{" ++ showEnv vz ++ show f ++ "}"
>   show (V i) = show i
>   show (e :$ es) = "(" ++ show e ++ (es >>= ((' ' :) . show)) ++")"
>   show (d := _) = "!" ++ d

> instance Show Fun where
>   show (Return e) = "." ++ show e
>   show (Lambda f) = "\\" ++ show f
>   show (Ignore f) = "/" ++ show f
>   show (Split f) = "^" ++ show f
>   show (Switch afs) =
>     "(" ++ intercalate " " (map (\ (a, f) -> a ++ show f) afs) ++ ")"
>   show (Handle afs f) =
>     "[" ++ intercalate " " (map (\ (a, f) -> a ++ show f) afs) ++ "]"
>     ++ show f

