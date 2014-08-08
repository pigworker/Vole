> module Compile where

> import Data.List

> import Stk
> import Syntax
> import Parser
> import GHCiVole

> type JS = String

> jp :: JS -> JS -> JS
> jp a d = "[" ++ a ++ "," ++ d ++ "]"

An expression compiles to a JS expression in terms of
a stack and an environment, delivering a stack-value pair.

> cExpFn :: Exp -> JS
> cExpFn e = "function(s,g){return(" ++ cExp ("s","g") e ++")}"

> cExp :: (JS, JS) -> Exp -> JS
> cExp (s, g) (A x) = jp s (show x)
> cExp (s, g) (a :& d) = cExp (jp s (jCar g d), g) a where
>   jCar g d = "{kind:\"Car\",env:" ++ g ++ ",go:" ++ cExpFn d ++"}"
> cExp (s, g) (d :/ f) = jp s (jClo g (cValz d) (cFun f)) where
>   jClo g b f = "{env:" ++ b ++ ".concat(" ++ g ++ "),fun:" ++ f ++ "}"
> cExp (s, g) (V i) = jp s (g ++ "[" ++ show i ++ "]")
> cExp (s, g) (f :$ as) = cExp (jp s (jFun g as), g) f where
>   jFun g as = "{kind:\"Fun\",clos:{env:" ++ g ++ ",goes:[" ++
>     intercalate "," (map cExpFn as) ++ "]}}"
> cExp (s, g) (d := _) = jp s ("voleMain(\"" ++ d ++ "\")")

> cValz :: Stk Val -> JS
> cValz vz = "[" ++ go vz ++ "]" where
>   go S0 = ""
>   go (S0 :< v) = cVal v
>   go (vz :< v) = cVal v ++ "," ++ go vz

> cVal :: Val -> JS
> cVal (AV x) = show x
> cVal (a :&: d) = jp (cVal a) (cVal d)
> cVal (d :/: f) = "{env:" ++ cValz d ++ ",fun:" ++ cFun f ++ "}"
> -- and the others don't have a syntax

> cFun :: Fun -> JS
> cFun (Return e) = cExpFn e
> cFun (h :? e) = "{han:" ++ cHan h ++ ",eat:" ++ cEat e ++ "}"

> cEat :: Eater Fun -> JS
> cEat e = "function(v){return" ++ go S0 e ["v"] ++ "}" where
>   go jz (Burp f) vs = jp ("[" ++ blat jz ++ "]") (cFun f)
>   go jz (Lambda f) (v : vs) = go (jz :< v) f vs
>   go jz (Ignore f) (_ : vs) = go jz f vs
>   go jz (Split f) (v : vs) = go jz f ((v ++ "[0]") : (v ++ "[1]") : vs)
>   go jz (Case afs) (v : vs) =
>     "{" ++ intercalate "," (map br afs) ++ "}[" ++ v ++ "]()" where
>     br (a, f) = a ++ ":function(){return " ++ go jz f vs ++ "}"
>   blat S0 = ""
>   blat (S0 :< j) = j
>   blat (jz :< j) = j ++ "," ++ blat jz

> cHan :: Han -> JS
> cHan (Han aes) = "{dlers:{" ++ intercalate "," (map ha aes) ++ "}}" where
>   ha (a, e) = a ++ ":" ++ cEat e
> cHan (Can as) = "{gabout:{" ++ intercalate "," (map ca as) ++ "}}" where
>   ca a = a ++ ":null"

> cDefs :: String -> String
> cDefs s = blat (mkDefs s) where
>   blat ds = "function voleMain(f){return {" ++
>     intercalate "," (map def ds) ++ "}[f]}"
>   def (f, v) = f ++ ":" ++ cVal v
