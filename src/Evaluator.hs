module Evaluator where

import List
import Char
import Types

definitionEq sym (PriDef s d) = sym == s
symbolBound sym env = find (\b -> definitionEq sym b) env

resolve sym env = case symbolBound sym env of
   Just (PriDef s v) -> v
   Nothing -> PriError("symbol unbound")

replaceOneBy fn a b (x : xs) =
   if fn a x
   then b : xs
   else x : replaceOneBy fn a b xs
replaceOneBy fn a b [] = []

bindOne sym exp env = (PriDef sym exp) : env
rebindOne sym exp env = replaceOneBy definitionEq sym (PriDef sym exp) env

bind params args env =
   foldl (\ e (sym, exp) ->
            case symbolBound sym e of
               Just _ -> rebindOne sym exp e
               Nothing -> bindOne sym exp e)
      env (zip params args)

uniOp Not (PriBool x)                   = PriBool (not x)
uniOp Neg (PriInt x)                    = PriInt (negate x)
uniOp Neg (PriFloat x)                  = PriFloat (negate x)
uniOp _ _                               = PriError("type mismatch")

binOp Eq x y                            = PriBool(x == y)
binOp Ne x y                            = PriBool(not $ x == y)
binOp Add (PriInt x) (PriInt y)         = PriInt(x + y)
binOp Add (PriInt x) (PriFloat y)       = PriFloat(fromIntegral x + y)
binOp Add (PriFloat x) (PriInt y)       = PriFloat(x + fromIntegral y)
binOp Add (PriFloat x) (PriFloat y)     = PriFloat(x + y)
binOp Sub (PriInt x) (PriInt y)         = PriInt(x - y)
binOp Sub (PriInt x) (PriFloat y)       = PriFloat(fromIntegral x - y)
binOp Sub (PriFloat x) (PriInt y)       = PriFloat(x - fromIntegral y)
binOp Sub (PriFloat x) (PriFloat y)     = PriFloat(x - y)
binOp Mul (PriInt x) (PriInt y)         = PriInt(x * y)
binOp Mul (PriInt x) (PriFloat y)       = PriFloat(fromIntegral x * y)
binOp Mul (PriFloat x) (PriInt y)       = PriFloat(x * fromInteger (toInteger y))
binOp Mul (PriFloat x) (PriFloat y)     = PriFloat(x * y)
binOp Div (PriInt x) (PriInt y)         = PriFloat(fromIntegral x / fromIntegral y)
binOp Div (PriInt x) (PriFloat y)       = PriFloat(fromIntegral x / y)
binOp Div (PriFloat x) (PriInt y)       = PriFloat(x / fromIntegral y)
binOp Div (PriFloat x) (PriFloat y)     = PriFloat(x / y)
binOp Mod (PriInt x) (PriInt y)         = PriInt(x `mod` y)
binOp Lt (PriInt x) (PriInt y)          = PriBool(x < y)
binOp Lt (PriFloat x) (PriFloat y)      = PriBool(x < y)
binOp Lt (PriInt x) (PriFloat y)        = PriBool(fromIntegral x < y)
binOp Lt (PriFloat x) (PriInt y)        = PriBool(x < fromIntegral y)
binOp Gt (PriInt x) (PriInt y)          = PriBool(x > y)
binOp Gt (PriFloat x) (PriFloat y)      = PriBool(x > y)
binOp Gt (PriInt x) (PriFloat y)        = PriBool(fromIntegral x > y)
binOp Gt (PriFloat x) (PriInt y)        = PriBool(x > fromIntegral y)
binOp Lte (PriInt x) (PriInt y)         = PriBool(x <= y)
binOp Lte (PriFloat x) (PriFloat y)     = PriBool(x <= y)
binOp Lte (PriInt x) (PriFloat y)       = PriBool(fromIntegral x <= y)
binOp Lte (PriFloat x) (PriInt y)       = PriBool(x <= fromIntegral y)
binOp Gte (PriInt x) (PriInt y)         = PriBool(x >= y)
binOp Gte (PriFloat x) (PriFloat y)     = PriBool(x >= y)
binOp Gte (PriInt x) (PriFloat y)       = PriBool(fromIntegral x  >= y)
binOp Gte (PriFloat x) (PriInt y)       = PriBool(x >= fromIntegral y)
binOp And (PriBool x) (PriBool y)       = PriBool(x && y)
binOp Or (PriBool x) (PriBool y)        = PriBool(x || y)
binOp App (PriList xs1) (PriList xs2)   = PriList(xs1 ++ xs2)
binOp App (PriString s1) (PriString s2) = PriString(s1 ++ s2)
binOp Rge (PriInt x) (PriInt y)         = PriList(map (\a -> PriInt(a)) [x..y])
binOp Cons lhs (PriList xs)             = PriList(lhs : xs)
binOp Cons (PriChar c) (PriString s)    = PriString(c : s)
binOp _ _ _                             = PriError("type mismatch")

car (PriList (x:_))                     = x
car (PriList [])                        = PriList []
car (PriString s)                       = PriChar(s !! 0)
car _                                   = PriError("type mismatch")

cdr (PriList (_:xs))                    = PriList(xs)
cdr (PriList [])                        = PriList []
cdr (PriString (_:xs))                  = PriString(xs)
cdr _                                   = PriError("type mismatch")

len (PriList xs)                        = PriInt(length xs)
len (PriString s)                       = PriInt(length s)
len _                                   = PriError("type mismatch")

at (PriList xs) (PriInt i)              = xs!!i
at _ _                                  = PriError("type mismatch")

cast (PriInt i) (PriType TDecimal)      = PriFloat(fromIntegral  i)
cast (PriInt i) (PriType TString)       = PriString(show i)
cast (PriInt i) (PriType TBool)         = PriBool(if i <= 0 then False else True)
cast (PriFloat f) (PriType TInt)        = PriInt(truncate f)
cast (PriFloat f) (PriType TString)     = PriString(show f)
cast (PriBool b) (PriType TInt)         = PriInt(if b then 1 else 0)
cast (PriBool b) (PriType TBool)        = PriFloat(if b then 1.0 else 0.0)
cast (PriBool b) (PriType TString)      = PriString(if b then "true" else "false")
cast (PriChar c) (PriType TInt)         = PriInt(digitToInt c)
cast (PriChar c) (PriType TDecimal)     = PriFloat(fromIntegral $ digitToInt c)
cast (PriChar c) (PriType TString)      = PriString(show c)
cast _ _                                = PriError("invalid cast")

is (PriInt _) (PriType TInt)            = PriBool(True)
is (PriFloat _) (PriType TDecimal)      = PriBool(True)
is (PriChar _) (PriType TChar)          = PriBool(True)
is (PriBool _) (PriType TBool)          = PriBool(True)
is (PriString _) (PriType TString)      = PriBool(True)
is (PriList _) (PriType TList)          = PriBool(True)
is _ _                                  = PriBool(False)

isPrimitive (PriInt _)                  = True
isPrimitive (PriFloat _)                = True
isPrimitive (PriChar _)                 = True
isPrimitive (PriBool _)                 = True
isPrimitive (PriString _)               = True
isPrimitive (PriList xs)                = all isPrimitive xs
isPrimitive _                           = False

eval exp@(PriInt _) env                 = exp
eval exp@(PriFloat _) env               = exp
eval exp@(PriChar _) env                = exp
eval exp@(PriBool _) env                = exp
eval exp@(PriString _) env              = exp
eval exp@(PriClosure _ _ _) env         = exp
eval exp@(PriSymbol s) env              = eval (resolve exp env) env
eval exp@(PriList l) env                = if isPrimitive exp
                                          then exp
                                          else PriList(evlis l env)

eval exp@(PriIf _ _ _) env              = condition exp env
eval (PriLet s e1 e2) env               = plet s e1 e2 env
eval (PriVal _ e) env                   = eval e env
eval (PriLambda p b) env                = (PriClosure p b env)
eval (PriApply s a) env                 = apply (eval s env) (evlis a env) env
eval (PriUniop o arg) env               = uniOp o (eval arg env)
eval (PriBinop o lhs rhs) env           = binOp o (eval lhs env) (eval rhs env)
eval (PriHead e) env                    = car (eval e env)
eval (PriTail e) env                    = cdr (eval e env)
eval (PriLength e) env                  = len (eval e env)
eval (PriAt xs i) env                   = at (eval xs env) (eval i env)
eval (PriIs x t) env                    = is (eval x env) (eval t env)
eval (PriCast f t) env                  = cast (eval f env) (eval t env)
eval exp@(PriError s) env               = exp
eval exp@(PriType t) env                = exp

apply (PriClosure p b ce) args env      = eval b (bind p args ce)
apply _ _ _                             = PriError("attempt to apply non closure")

evlis lst env                           = map (\exp -> eval exp env) lst
plet sym exp1 exp2 env                  = eval exp2 (bind [sym] [exp1] env)

condition (PriIf p c a) env =
  if b then (eval c env) else (eval a env)
  where (PriBool b) = (eval p env)

topLevel = (PriDef (PriSymbol "int") (PriType TInt))
         : (PriDef (PriSymbol "float") (PriType TDecimal))
         : (PriDef (PriSymbol "char") (PriType TChar))
         : (PriDef (PriSymbol "bool") (PriType TBool))
         : (PriDef (PriSymbol "string") (PriType TString))
         : (PriDef (PriSymbol "list") (PriType TList))
         : (PriDef (PriSymbol "lambda") (PriType TLambda))
         : (PriDef (PriSymbol "newline") (PriChar '\n'))
         : (PriDef (PriSymbol "tab") (PriChar '\t'))
         : []