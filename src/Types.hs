module Types where

import List

data Typename = TInt | TDecimal | TChar | TBool | TList | TString | TLambda
data Uniop = Not | Neg
data Binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Gt | Gte | Lte | And | Or | App | Rge | Cons

data Expression =
    PriSymbol String
  | PriInt Int
  | PriFloat Float
  | PriChar Char
  | PriBool Bool
  | PriString String
  | PriList [Expression]
  | PriIf Expression Expression Expression
  | PriLet Expression Expression Expression
  | PriVal Expression Expression
  | PriLambda [Expression] Expression
  | PriClosure [Expression] Expression [Expression]
  | PriApply Expression [Expression]
  | PriBinop Binop Expression Expression
  | PriUniop Uniop Expression
  | PriHead Expression
  | PriTail Expression
  | PriLength Expression
  | PriShow Expression
  | PriAt Expression Expression
  | PriIs Expression Expression
  | PriCast Expression Expression
  | PriDef Expression Expression
  | PriError String
  | PriType Typename

instance Show Expression where
   show (PriSymbol s)      = show s
   show (PriInt i)         = show i
   show (PriFloat f)       = show f
   show (PriChar c)        = show c
   show (PriBool b)        = show b
   show (PriString s)      = show s
   show (PriList l)        = show l
   show (PriError s)       = show s
   show (PriType t)        = show "#<type>"
   show (PriLambda p b)    = show "#<lambda>"
   show (PriClosure p b e) = show "#<closure>"
   show _                  = show "#<builtin>"

instance Eq Expression where
   (PriInt x)       == (PriInt y)       = x == y
   (PriFloat x)     == (PriFloat y)     = x == y
   (PriInt x)       == (PriFloat y)     = fromIntegral x == y
   (PriFloat x)     == (PriInt y)       = x == fromIntegral y
   (PriChar x)      == (PriChar y)      = x == y
   (PriBool x)      == (PriBool y)      = x == y
   (PriSymbol x)    == (PriSymbol y)    = x == y
   (PriList (x:xs)) == (PriList (y:ys)) = (x == y) && ((PriList xs) == (PriList ys))
   (PriList [])     == (PriList [])     = True
   _                == _                = False