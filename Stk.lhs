> {-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

> module Stk where

> import Data.Monoid
> import Data.Foldable
> import Data.Traversable

I'd rather turn my code around than my head, so here is the basic
kit for stacks which grow on the right.

> data Stk x = S0 | Stk x :< x
>   deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
> infixl 4 :<

Stacks are, of course, a monoid with respect to concatenation.

> instance Monoid (Stk x) where
>   mempty = S0
>   mappend xz (yz :< y) = mappend xz yz :< y
>   mappend xz S0 = xz

The usual "fish" and "chips" operations support abacus-style concatenation.

> (<><) :: Stk x -> [x] -> Stk x
> xz <>< [] = xz
> xz <>< (x : xs) = (xz :< x) <>< xs
> infixl 4 <><

> (<>>) :: Stk x -> [x] -> [x]
> S0 <>> vs = vs
> (vz :< v) <>> vs = vz <>> (v : vs)
> infixr 4 <>>

And here's your unsafe projection!!

> pop :: Stk x -> Int -> x
> pop (_ :< x) 0 = x
> pop (xz :< _) i = pop xz (i - 1)
