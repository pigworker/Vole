> module Parser where

This module defines a parser for the concrete syntax for Vole.

> import Control.Applicative
> import Control.Monad
> import Data.Char

> import Stk
> import Syntax

We need to cache the values of definitions, so let us assume we know them
already.

> type Defs = [(String, Val)]

> newtype P x = P {parse :: Defs -> Stk String -> String -> Maybe (x, String)}

When processing a file, we can fill in the definitions with a dastardly bit
of Oxford-programming. Meanwhile, for a little humanity in this otherwise
barren territory, I assume also a stack of symbols which have been bound by
lambdas, just so that any humans writing Vole code aren't obliged to use
de Bruijn notation.

Here's the usual gubbins.

> instance Monad P where
>   return x = P $ \ ds xz s -> Just (x, s)
>   P f >>= k = P $ \ ds xz s -> do
>     (a, s) <- f ds xz s
>     parse (k a) ds xz s
> instance Applicative P where pure = return ; (<*>) = ap
> instance Functor P where fmap = ap . return
> instance Alternative P where
>   empty = P $ \ _ _ _ -> Nothing
>   P f <|> P g = P $ \ ds xz s -> f ds xz s <|> g ds xz s

As ever, we need the choosy chomper...

> pCh :: (Char -> Bool) -> P Char
> pCh p = P $ \ _ _ s -> case s of
>   c : s | p c -> Just (c, s)
>   _ -> Nothing

...from which we make some handy helpers.

> pQ :: Char -> P ()
> pQ c = () <$ pSpc <* pCh (== c) <* pSpc

> pSpc :: P ()
> pSpc = () <$ many (pCh isSpace)

We have a control operator to bind a variable...

> pBind :: String -> P x -> P x
> pBind x p = P $ \ ds xz s -> parse p ds (xz :< x) s

... and a "parser" which looks up a string we already have, to try
delivering a de Bruijn index.

> pVar :: String -> P Int
> pVar y = P $ \ _ xz -> go 0 xz where
>   go _ S0 = \ s -> Nothing
>   go i (_ :< x) | x == y = \ s -> Just (i, s)
>   go i (xz :< _) = go (i + 1) xz

As befits the Oxford style, the corresponding operation for *defined* symbols
is sunnily optimistic.

> pDef :: String -> P Val
> pDef f = P $ \ ds _ s -> let Just h = lookup f ds in Just (h, s)

Atoms avoid space and special characters and do not begin with a digit.

> special :: String
> special = "!,\\/^.[](){}"

> isAtomic :: Char -> Bool
> isAtomic c = not (isSpace c || elem c special)

> pAtom :: P String
> pAtom = (:) <$ pSpc <*>
>   pCh (\ c -> isAtomic c && not (isDigit c)) <*>
>   many (pCh isAtomic)

Here, I save a little by noticing the common structure to `Val` and `Exp`.

> class Show x => Lispy x where
>   atom    :: String -> x
>   cons    :: x -> x -> x
>   thunk   :: Stk Val -> Fun -> x
>   parser  :: P x

This bit is the common chunk that works for both.

> pValue :: Lispy x => P x
> pValue
>   =    atom <$> pAtom
>   <|>  id <$ pQ '[' <*> pBra
>   <|>  thunk <$ pQ '{' <*> ((S0 <><) <$> many (pQ '!' *> parser)) <*>
>          pFun <* pQ '}'
>   where
>     pBra = atom "" <$ pQ ']' <|> cons <$> parser <*> pCdr where
>     pCdr = id <$ pQ ',' <*> parser <* pQ ']' <|> pBra

That's all we need for `Val` (because I don't currently let you write
continuations by hand).

> instance Lispy Val where
>   atom = AV;  cons = (:&:);  thunk = (:/:);  parser  = pValue

For `Exp`, we need to try a little harder.

> instance Lispy Exp where
>   atom = A;  cons = (:&);  thunk = (:/)
>   parser
>     =    (pAtom >>= \ a -> V <$> pVar a) -- shadows atoms
>     <|>  pValue
>     <|>  (pQ '!' *> pAtom >>= \ a -> (a :=) <$> pDef a)
>     <|>  (:$) <$ pQ '(' <*> parser <*> many parser <* pQ ')'

And we do our own thing for `Fun`.

> pFun :: P Fun
> pFun
>   =    Return <$ pQ '.' <*> parser
>   <|>  Lambda <$ pQ '\\' <*> ((pAtom <|> pure "") >>= \ a -> pBind a pFun)
>   <|>  Ignore <$ pQ '/' <*> pFun
>   <|>  Split <$ pQ '^' <*> pFun
>   <|>  Switch <$ pQ '(' <*> many ((,) <$> pAtom <*> pFun) <* pQ ')'
>   <|>  Handle <$ pQ '[' <*>
>          many ((,) <$ pSpc <*> pAtom <*> pFun) <* pQ ']' <*> pFun
