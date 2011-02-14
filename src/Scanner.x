{
module Scanner where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
@string = \" $alpha* \"
@char = \' $alpha \'
@float = $digit+ \. $digit+
@comment = \#*.*\n

tokens :-

  $white+				;
  "++"                  { \s -> TAppend }
  "+"                   { \s -> TPlus }
  "-"                   { \s -> TMinus }
  "*"                   { \s -> TTimes }
  "/"                   { \s -> TDivide }
  "("                   { \s -> TLParen }
  ")"                   { \s -> TRParen }
  "["                   { \s -> TLSquare }
  "]"                   { \s -> TRSquare }
  "="                   { \s -> TDef }
  "not"                 { \s -> TNot }
  "<"                   { \s -> TLt }
  ">"                   { \s -> TGt }
  ">="                  { \s -> TGe }
  "<="                  { \s -> TLe }
  "=="                  { \s -> TEq }
  "!="                  { \s -> TNe }
  "and"                 { \s -> TAnd }
  "or"                  { \s -> TOr }
  ".."                  { \s -> TRange }
  "mod"                 { \s -> TMod }
  ","                   { \s -> TComma }
  ";"                   { \s -> TSemicolon }
  "&"                   { \s -> TBAnd }
  "|"                   { \s -> TBOr }
  "^"                   { \s -> TBXor }
  "~"                   { \s -> TBNot }
  "<<"                  { \s -> TBLShift }
  ">>"                  { \s -> TBRShift }
  "if"			        { \s -> TIf }
  "then"                { \s -> TThen }
  "else"		        { \s -> TElse }
  "let"                 { \s -> TLet }
  "val"                 { \s -> TVal }
  "in"                  { \s -> TIn }
  "fn"                  { \s -> TFn }
  "true"		        { \s -> TTrue }
  "false"		        { \s -> TFalse }
  "is"                  { \s -> TIs }
  "as"                  { \s -> TAs }
  "at"                  { \s -> TAt }
  "::"                  { \s -> TCons }
  "Head"                { \s -> THead }
  "Tail"                { \s -> TTail }
  "Show"                { \s -> TShow }
  "Type"                { \s -> TType }
  "length"		        { \s -> TLength }
  @string 	       	    { \s -> TStringLiteral (init (tail s)) }
  @char                 { \s -> TCharLiteral (head (init (tail s))) }
  @float                { \s -> TFloatLiteral (read s) }
  $digit+		        { \s -> TIntLiteral (read s) }
  $alpha[$alpha $digit \_ \']*	{ \s -> TIdent s }

{
data Token =
     TAppend                 |
     TPlus                   |
     TMinus                  |
     TTimes                  |
     TDivide                 |
     TLParen                 |
     TRParen                 |
     TLSquare                |
     TRSquare                |
     TDef                    |
     TNot                    |
     TLt                     |
     TGt                     |
     TGe                     |
     TLe                     |
     TEq                     |
     TNe                     |
     TAnd                    |
     TOr                     |
     TRange                  |
     TMod                    |
     TComma                  |
     TSemicolon              |
     TBAnd                   |
     TBOr                    |
     TBXor                   |
     TBNot                   |
     TBLShift                |
     TBRShift                |
     TIf                     |
     TThen                   |
     TElse                   |
     TLet                    |
     TVal                    |
     TSeq                    |
     TIn                     |
     TFn                     |
     TTrue                   |
     TFalse                  |
     TIs                     |
     TAs                     |
     TAt                     |
     TCons                   |
     THead                   |
     TTail                   |
     TShow                   |
     TRnd                    |
     TType                   |
     TLength                 |
     TStringLiteral String   |
     TCharLiteral Char       |
     TIntLiteral Int         |
     TFloatLiteral Float     |
     TIdent String
	deriving (Eq,Show)

--main = do
--  s <- getContents
--  print (alexScanTokens s)

}