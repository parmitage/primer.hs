{
module Scanner where

import Types
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]
$any = [^ \t \n]
@string = \" $any* \"
@char = \' $any \'
@float = $digit+ \. $digit+

tokens :-

  $white+				        ;
  "++"                          { \s -> TAppend }
  "+"                           { \s -> TPlus }
  "-"                           { \s -> TMinus }
  "*"                           { \s -> TTimes }
  "/"                           { \s -> TDivide }
  "("                           { \s -> TLParen }
  ")"                           { \s -> TRParen }
  "["                           { \s -> TLSquare }
  "]"                           { \s -> TRSquare }
  "="                           { \s -> TDef }
  "not"                         { \s -> TNot }
  "<"                           { \s -> TLt }
  ">"                           { \s -> TGt }
  ">="                          { \s -> TGe }
  "<="                          { \s -> TLe }
  "=="                          { \s -> TEq }
  "!="                          { \s -> TNe }
  "and"                         { \s -> TAnd }
  "or"                          { \s -> TOr }
  ".."                          { \s -> TRange }
  "mod"                         { \s -> TMod }
  ","                           { \s -> TComma }
  "&"                           { \s -> TBAnd }
  "|"                           { \s -> TBOr }
  "^"                           { \s -> TBXor }
  "~"                           { \s -> TBNot }
  "<<"                          { \s -> TBLShift }
  ">>"                          { \s -> TBRShift }
  "if"			                { \s -> TIf }
  "then"                        { \s -> TThen }
  "else"		                { \s -> TElse }
  "let"                         { \s -> TLet }
  "in"                          { \s -> TIn }
  "fn"                          { \s -> TFn }
  "true"		                { \s -> TTrue }
  "false"		                { \s -> TFalse }
  "is"                          { \s -> TIs }
  "as"                          { \s -> TAs }
  "at"                          { \s -> TAt }
  "::"                          { \s -> TCons }
  "head"                        { \s -> THead }
  "tail"                        { \s -> TTail }
  "show"                        { \s -> TShow }
  "type"                        { \s -> TType }
  "length"		                { \s -> TLength }
  "#" $any* \n?                 ;
  @string 	       	            { \s -> TStringLiteral (init (tail s)) }
  @char                         { \s -> TCharLiteral (head (init (tail s))) }
  @float                        { \s -> TFloatLiteral (read s) }
  $digit+		                { \s -> TIntLiteral (read s) }
  $alpha[$alpha $digit \_ \']*	{ \s -> TIdent s }