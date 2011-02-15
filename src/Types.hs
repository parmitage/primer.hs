module Types where

import List

data Typename = TInt | TDecimal | TChar | TBool | TList | TString | TLambda
data Uniop = Not | Neg
data Binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Lt | Gt | Gte | Lte | And | Or | Append | Range | Cons

data Expression =
    PriSymbol String
  | PriInteger Int
  | PriDecimal Float
  | PriChar Char
  | PriBoolean Bool
  | PriString String
  | PriList [Expression]
  | PriIf Expression Expression Expression
  | PriLet Expression Expression Expression
  | PriVal Expression Expression
  | PriLambda [Expression] Expression
  | PriClosure [Expression] Expression [Expression]
  | PriApply Expression [Expression]
  | PriBinaryOperator Binop Expression Expression
  | PriUnaryOperator Uniop Expression
  | PriHead Expression
  | PriTail Expression
  | PriLength Expression
  | PriShow Expression
  | PriAt Expression Expression
  | PriIs Expression Expression
  | PriCast Expression Expression
  | PriDef Expression Expression
  | PriNop

instance Show Expression where
   show (PriSymbol s)      = show s
   show (PriInteger i)     = show i
   show (PriDecimal f)     = show f
   show (PriChar c)        = show c
   show (PriBoolean b)     = show b
   show (PriString s)      = show s
   show (PriList l)        = show (intersperse (PriString ", ") l)
   show (PriLambda p b)    = show "#<lambda>"
   show (PriClosure p b e) = show "#<closure>"
   show _                  = show "#<builtin>"

instance Eq Expression where
   (PriInteger x)   == (PriInteger y)   = x == y
   (PriDecimal x)   == (PriDecimal y)   = x == y
   (PriInteger x)   == (PriDecimal y)   = fromIntegral x == y
   (PriDecimal x)   == (PriInteger y)   = x == fromIntegral y
   (PriChar x)      == (PriChar y)      = x == y
   (PriBoolean x)   == (PriBoolean y)   = x == y
   (PriSymbol x)    == (PriSymbol y)    = x == y
   (PriList (x:xs)) == (PriList (y:ys)) = (x == y) && ((PriList xs) == (PriList ys))
   (PriList [])     == (PriList [])     = True
   _                == _                = False