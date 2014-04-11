> module Syntax where

> import Data.List

> import Stk

The first-order data values are atoms or cons-cells, but unlike LISP, the
higher-order values are black-boxed: they are closures or
continuations.

> data Val
>   = AV String                 -- atom
>   | Val :&: Val               -- cons
>   | Stk Val :/: Fun Han Body  -- closure
>   | K [Layer]                 -- continuation

Expressions have the same stuff as values (apart from continuations, which
should be generated only by the machine. We also have variables (de Bruijn
indices), application, and appeal to explicit cached definition.

> data Exp
>   = A String                 -- atom
>   | Exp :& Exp               -- cons
>   | Stk Val :/ Fun Han Body  -- closure
>   | V Int                    -- de Bruijn index
>   | Exp :$ [Exp]             -- application
>   | String := Val            -- defined thing and its value

A function eats a list of argument values, growing an environment, the while.
The catch is, some of the function's argument expressions might invoke effects
which we should handle.

> data Fun h x                      -- h is what handling we're allowed
>   = Return x                      -- no more arguments, compute the return
>   | h :? Eater (Fun h x)          -- possibly handling instead, eat an arg

> data Body = Body Exp
> data NoH = NoH
> data Han = Han [(String, Fun NoH (Fun Han Body))]

> data Eater x                    -- x is what happens afterwards
>   = Burp x                      -- end of meal
>   | Lambda (Eater x)            -- push first arg onto environment
>   | Ignore (Eater x)            -- drop first arg
>   | Split (Eater x)             -- replace first arg by its car and cdr
>   | Case [(String, Eater x)]    -- remove and branch on atomic first arg

There are some invariants not managed here:
  * you have to know when to `Split` and when to `Case`
  * you have to mind the arity of the function yourself
  * ensuring complete consumption is asking for GADTs

The stack layers reflect what we might be in the middle of. It's a *dissection*.

> type Closure = (Stk Val, Exp)
> data Layer
>   = Car Closure                      -- eval a car, with cdr pending
>   | Cdr Val                          -- eval a cdr, with car parked
>   | Fun [Closure]                    -- eval a function, arguments pending
>   | Eff String (Stk Val, [Closure])  -- eval an arg of an effect
>   | Eat (Stk Val) Han (Eater (Fun Han Body)) [Closure]
>     -- eval an arg, ready to handle effects or to consume a value
>   deriving Show

Boring `Show` implementations.

> instance Show Val where
>   show (AV "") = "[]"
>   show (AV a) = a
>   show (v :&: d) = "[" ++ show v ++ cdr d where
>     cdr (AV "") = "]"
>     cdr (v :&: d) = " " ++ show v ++ cdr d
>     cdr v = "." ++ show v ++ "]"
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
>     cdr e = "." ++ show e ++ "]"
>   show (vz :/ f) = "{" ++ showEnv vz ++ show f ++ "}"
>   show (V i) = show i
>   show (e :$ es) = "(" ++ show e ++ (es >>= ((' ' :) . show)) ++")"
>   show (d := _) = "!" ++ d

> instance (Show h, Show x) => Show (Fun h x) where
>   show (Return e) = show e
>   show (h :? f) = show h ++ show f

> instance Show Body where show (Body e) = '.' : show e

> instance Show NoH where show _ = ""

> instance Show Han where
>   show (Han []) = ""
>   show (Han afs) = "[" ++ intercalate " " (map jo afs) ++ "]" where
>     jo (a, f) = a ++ show f

> instance Show x => Show (Eater x) where
>   show (Burp x) = show x
>   show (Lambda f) = "\\" ++ show f
>   show (Ignore f) = "/" ++ show f
>   show (Split f) = "^" ++ show f
>   show (Case afs) =
>     "(" ++ intercalate " " (map (\ (a, f) -> a ++ show f) afs) ++ ")"


