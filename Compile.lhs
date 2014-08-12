> module Compile where

> import Data.List
> import Control.Applicative

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

> cExpVal :: JS -> Exp -> Maybe JS
> cExpVal g (A x) = Just (show x)
> cExpVal g (N n) = Just (show n)
> cExpVal g (a :& d) = jp <$> cExpVal g a <*> cExpVal g d
> cExpVal g (V i) = Just (g ++ "[" ++ show i ++ "]")
> cExpVal g (d := _) = Just ("voleMain(\"" ++ d ++ "\")")
> cExpVal _ _ = Nothing

> cExp :: (JS, JS) -> Exp -> JS
> cExp (s, g) e = case cExpVal g e of
>   Just v -> jp s v
>   Nothing -> case e of
>     (a :& d) -> case cExpVal g a of
>       Nothing  -> cExp (jp s (jCar g d), g) a
>       Just a   -> cExp (jp s (jCdr a), g) d
>     (d :/ f) -> jp s (jClo g (cValz d) (cFun f))
>     (f :$ as) -> cExp (jp s (jFun g as), g) f
>     (Let e h f) -> cExp (jp s (jEat g h f), g) e
>  where
>   jCar g d = "{kind:\"Car\",env:" ++ g ++ ",go:" ++ cExpFn d ++"}"
>   jCdr a = "{kind:\"Cdr\",car:" ++ a ++ "}"
>   jClo g b f = "{env:" ++ b ++ ".concat(" ++ g ++ "),fun:" ++ f ++ "}"
>   jFun g as = "{kind:\"Fun\",clos:{env:" ++ g ++ ",goes:[" ++
>     intercalate "," (map cExpFn as) ++ "]}}"
>   jEat g h f = "{kind:\"Eat\",env:" ++ g ++ ",han:" ++ cHan h ++
>     ",eat:" ++ cEat f ++ ",clos:{env:[],goes:[]}}"

> cValz :: Stk Val -> JS
> cValz vz = "[" ++ go vz ++ "]" where
>   go S0 = ""
>   go (S0 :< v) = cVal v
>   go (vz :< v) = cVal v ++ "," ++ go vz

> cVal :: Val -> JS
> cVal (AV x) = show x
> cVal (NV n) = show n
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
> cHan (Han []) = "null"
> cHan (Han aes) = "{dlers:{" ++ intercalate "," (map ha aes) ++ "}}" where
>   ha (a, e) = a ++ ":" ++ cEat e
> cHan (Can as) = "{gabout:{" ++ intercalate "," (map ca as) ++ "}}" where
>   ca a = a ++ ":null"

> cDefs :: String -> String
> cDefs s = blat (mkDefs s) where
>   blat ds = "function voleMain(f){return {" ++
>     intercalate "," (map def ds) ++ "}[f]}"
>   def (f, v) = f ++ ":" ++ cVal v
