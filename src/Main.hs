module Main (main) where

import List
import Char
import System.IO
import System.Environment
import System.Exit
import Types
import Scanner
import Parser

symbolEq (PriSymbol s1) (PriSymbol s2) = s1 == s2
definitionEq sym (PriDef s d) = symbolEq sym s
symbolBound sym env = find (\b -> definitionEq sym b) env

resolve sym env = case symbolBound sym env of
   Just (PriDef s v) -> v
   Nothing -> error "symbol unbound"

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

unaryOp Not (PriBoolean x) = (PriBoolean (not x))
unaryOp Neg (PriInteger x) = (PriInteger (negate x))
unaryOp Neg (PriDecimal x) = (PriDecimal (negate x))

binaryOp Add (PriInteger x) (PriInteger y) = PriInteger(x + y)
binaryOp Add (PriInteger x) (PriDecimal y) = PriDecimal(fromIntegral x + y)
binaryOp Add (PriDecimal x) (PriInteger y) = PriDecimal(x + fromIntegral y)
binaryOp Add (PriDecimal x) (PriDecimal y) = PriDecimal(x + y)

binaryOp Sub (PriInteger x) (PriInteger y) = PriInteger(x - y)
binaryOp Sub (PriInteger x) (PriDecimal y) = PriDecimal(fromIntegral x - y)
binaryOp Sub (PriDecimal x) (PriInteger y) = PriDecimal(x - fromIntegral y)
binaryOp Sub (PriDecimal x) (PriDecimal y) = PriDecimal(x - y)

binaryOp Mul (PriInteger x) (PriInteger y) = PriInteger(x * y)
binaryOp Mul (PriInteger x) (PriDecimal y) = PriDecimal(fromIntegral x * y)
binaryOp Mul (PriDecimal x) (PriInteger y) = PriDecimal(x * fromInteger (toInteger y))
binaryOp Mul (PriDecimal x) (PriDecimal y) = PriDecimal(x * y)

binaryOp Div (PriInteger x) (PriInteger y) = PriDecimal(fromIntegral x / fromIntegral y)
binaryOp Div (PriInteger x) (PriDecimal y) = PriDecimal(fromIntegral x / y)
binaryOp Div (PriDecimal x) (PriInteger y) = PriDecimal(x / fromIntegral y)
binaryOp Div (PriDecimal x) (PriDecimal y) = PriDecimal(x / y)

binaryOp Mod (PriInteger x) (PriInteger y) = PriInteger(x `mod` y)

binaryOp Eq (PriInteger x) (PriInteger y) = PriBoolean(x == y)
binaryOp Eq (PriDecimal x) (PriDecimal y) = PriBoolean(x == y)
binaryOp Eq (PriInteger x) (PriDecimal y) = PriBoolean(fromIntegral x == y)
binaryOp Eq (PriDecimal x) (PriInteger y) = PriBoolean(x == fromIntegral y)
binaryOp Eq (PriChar x) (PriChar y) = PriBoolean(x == y)
binaryOp Eq (PriBoolean x) (PriBoolean y) = PriBoolean(x == y)
binaryOp Eq (PriSymbol x) (PriSymbol y) = PriBoolean(x == y)
binaryOp Eq _ _ = PriBoolean(False)

binaryOp Ne (PriInteger x) (PriInteger y) = PriBoolean(x /= y)
binaryOp Ne (PriDecimal x) (PriDecimal y) = PriBoolean(x /= y)
binaryOp Ne (PriInteger x) (PriDecimal y) = PriBoolean(fromIntegral x /= y)
binaryOp Ne (PriDecimal x) (PriInteger y) = PriBoolean(x /= fromIntegral y)
binaryOp Ne (PriChar x) (PriChar y) = PriBoolean(x /= y)
binaryOp Ne (PriBoolean x) (PriBoolean y) = PriBoolean(x /= y)
binaryOp Ne (PriSymbol x) (PriSymbol y) = PriBoolean(x /= y)
binaryOp Ne _ _ =  PriBoolean(True)

binaryOp Lt (PriInteger x) (PriInteger y) = PriBoolean(x < y)
binaryOp Lt (PriDecimal x) (PriDecimal y) = PriBoolean(x < y)
binaryOp Lt (PriInteger x) (PriDecimal y) = PriBoolean(fromIntegral x < y)
binaryOp Lt (PriDecimal x) (PriInteger y) = PriBoolean(x < fromIntegral y)

binaryOp Gt (PriInteger x) (PriInteger y) = PriBoolean(x > y)
binaryOp Gt (PriDecimal x) (PriDecimal y) = PriBoolean(x > y)
binaryOp Gt (PriInteger x) (PriDecimal y) = PriBoolean(fromIntegral x > y)
binaryOp Gt (PriDecimal x) (PriInteger y) = PriBoolean(x > fromIntegral y)

binaryOp Lte (PriInteger x) (PriInteger y) = PriBoolean(x <= y)
binaryOp Lte (PriDecimal x) (PriDecimal y) = PriBoolean(x <= y)
binaryOp Lte (PriInteger x) (PriDecimal y) = PriBoolean(fromIntegral x <= y)
binaryOp Lte (PriDecimal x) (PriInteger y) = PriBoolean(x <= fromIntegral y)

binaryOp Gte (PriInteger x) (PriInteger y) = PriBoolean(x >= y)
binaryOp Gte (PriDecimal x) (PriDecimal y) = PriBoolean(x >= y)
binaryOp Gte (PriInteger x) (PriDecimal y) = PriBoolean(fromIntegral x  >= y)
binaryOp Gte (PriDecimal x) (PriInteger y) = PriBoolean(x >= fromIntegral y)

binaryOp And (PriBoolean x) (PriBoolean y) = PriBoolean(x && y)

binaryOp Or (PriBoolean x) (PriBoolean y) = PriBoolean(x || y)

binaryOp Append (PriList xs1) (PriList xs2) = PriList(xs1 ++ xs2)
binaryOp Append (PriString s1) (PriString s2) = PriString(s1 ++ s2)

binaryOp Range (PriInteger x) (PriInteger y) = PriList(map (\a -> PriInteger(a)) [x..y])

binaryOp Cons lhs (PriList xs) = PriList(lhs : xs)
binaryOp Cons (PriChar c) (PriString s) = PriString(c : s)

binaryOp _ _ _ = error "type mismatch"

car (PriList (x:_)) = x
car (PriString s) = PriChar(s !! 0)
car _ = error "type mismatch"

cdr (PriList (_:xs)) = PriList(xs)
cdr (PriString (_:xs)) = PriString(xs)
cdr _ = error "type mismatch"

len (PriList xs) = PriInteger(length xs)
len (PriString s) = PriInteger(length s)
len _ = error "type mismatch"

at (PriList xs) (PriInteger i) = xs!!i
at _ _ = error "type mismatch"

cast (PriInteger i) (PriDecimal f) = PriDecimal(fromIntegral  i)
cast (PriInteger i) (PriString s) = PriString(show i)
cast (PriInteger i) (PriBoolean b) = PriBoolean(if i <= 0 then False else True)
cast (PriDecimal f) (PriInteger i) = PriInteger(truncate f)
cast (PriDecimal f) (PriString s) = PriString(show f)
cast (PriBoolean b) (PriInteger i) = PriInteger(if b then 1 else 0)
cast (PriBoolean b) (PriDecimal f) = PriDecimal(if b then 1.0 else 0.0)
cast (PriBoolean b) (PriString s) = PriString(if b then "true" else "false")
cast (PriChar c) (PriInteger i) = PriInteger(digitToInt c)
cast (PriChar c) (PriDecimal f) = PriDecimal(fromIntegral $ digitToInt c)
cast (PriChar c) (PriString s) = PriString(show c)
cast _ _ = error "type mismatch"

is (PriInteger _) (PriType TInt) = PriBoolean(True)
is (PriDecimal _) (PriType TDecimal) = PriBoolean(True)
is (PriChar _) (PriType TChar) = PriBoolean(True)
is (PriBoolean _) (PriType TBool) = PriBoolean(True)
is (PriString _) (PriType TString) = PriBoolean(True)
is (PriList _) (PriType TList) = PriBoolean(True)
is _ _ = PriBoolean(False)

isPrimitive (PriInteger _) = True
isPrimitive (PriDecimal _) = True
isPrimitive (PriChar _) = True
isPrimitive (PriBoolean _) = True
isPrimitive (PriString _) = True
isPrimitive (PriList xs) = all isPrimitive xs
isPrimitive _ = False

eval exp@(PriInteger _) env = exp
eval exp@(PriDecimal _) env = exp
eval exp@(PriChar _) env = exp
eval exp@(PriBoolean _) env = exp
eval exp@(PriString _) env = exp
eval exp@(PriClosure _ _ _) env = exp
eval exp@(PriType _) env = exp
eval exp@(PriSymbol s) env = eval (resolve exp env) env
eval exp@(PriList l) env = if isPrimitive exp then exp else PriList(evlis l env)
eval exp@(PriIf _ _ _) env = condition exp env
eval exp@(PriLet s e1 e2) env = plet s e1 e2 env
eval exp@(PriVal _ e) env = eval e env
eval exp@(PriLambda p b) env = (PriClosure p b env)
eval exp@(PriApply s a) env = apply (eval s env) (evlis a env) env
eval exp@(PriUnaryOperator o arg) env = unaryOp o (eval arg env)
eval exp@(PriBinaryOperator o lhs rhs) env = binaryOp o (eval lhs env) (eval rhs env)
eval exp@(PriHead e) env = car (eval e env)
eval exp@(PriTail e) env = cdr (eval e env)
eval exp@(PriLength e) env = len (eval e env)
eval exp@(PriAt xs i) env = at (eval xs env) (eval i env)
eval exp@(PriIs x t) env = is (eval x env) (eval t env)
eval exp@(PriCast f t) env = cast (eval f env) (eval t env)

apply (PriClosure p b ce) args env = eval b (bind p args ce)

evlis lst env = map (\exp -> eval exp env) lst

plet sym exp1 exp2 env = eval exp2 (bind [sym] [exp1] env)

condition (PriIf p c a) env = if b then (eval c env) else (eval a env)
   where (PriBoolean b) = (eval p env)
condition _ env = error "type mismatch"

topLevel = (PriDef (PriSymbol "int") (PriType TInt))
         : (PriDef (PriSymbol "PriDecimal") (PriType TDecimal))
         : (PriDef (PriSymbol "char") (PriType TChar))
         : (PriDef (PriSymbol "PriBoolean") (PriType TBool))
         : (PriDef (PriSymbol "string") (PriType TString))
         : (PriDef (PriSymbol "list") (PriType TList))
         : (PriDef (PriSymbol "lambda") (PriType TLambda))
         : (PriDef (PriSymbol "newline") (PriChar '\n'))
         : (PriDef (PriSymbol "tab") (PriChar '\t'))
         : []

repl env (x:xs) = do
  case x of
       PriVal s e -> repl ((PriDef s e) : env) xs
       _          -> do
                        let res = eval x env
                        putStrLn $ show res
                        repl env xs
repl env _ = do exitSuccess

--main = do
--  inStr <- getContents
--  let ast = primer (alexScanTokens inStr)
--  repl defs ast

main = do
  (fname:_) <- getArgs
  h <- openFile fname ReadMode
  str <- hGetContents h
  let ast = primer (alexScanTokens str)
  repl topLevel ast