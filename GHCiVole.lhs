> module GHCiVole where

> import Control.Applicative

> import Stk
> import Syntax
> import Parser
> import Machine

Here's the Oxford program which parses a bunch of definitions assuming you
already know them. Ideal for an input file.

> mkDefs :: String -> Defs
> mkDefs s = defs where
>   Just (src, []) = parse (many pDef) defs S0 s
>   defs = [(f, run S0 (S0, e)) | (f, e) <- src]
>   pDef = (,) <$ pSpc <* pQ '!' <*> pAtom <*> parser

Given some definitions, try evaluating an expression!

> try :: Defs -> String -> Val
> try defs s = run S0 (S0, e) where
>   Just (e, "") = parse parser defs S0 s
