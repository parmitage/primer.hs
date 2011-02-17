{
module Parser where
      
import Scanner
import Types
}

%name primer
%tokentype { Token }
%error { parseError }
%token

'='                     { TDef }
integer                 { TIntLiteral $$ }
float                   { TFloatLiteral $$ }
char                    { TCharLiteral $$ }
"true"                  { TTrue }
"false"                 { TFalse }
string                  { TStringLiteral $$ }
ident                   { TIdent $$ }
"let"                   { TLet }
"fn"                    { TFn }
"if"                    { TIf }
"then"                  { TThen }
"else"                  { TElse }
"::"                    { TCons }
'+'                     { TPlus }
'-'                     { TMinus }
'*'                     { TTimes }
'/'                     { TDivide }
"mod"                   { TMod }
'<'                     { TLt }
'>'                     { TGt }
"<="                    { TLe }
">="                    { TGe }
"=="                    { TEq }
"!="                    { TNe }
"and"                   { TAnd }
"or"                    { TOr }
"not"                   { TNot }
"++"                    { TAppend }
".."                    { TRange }
'~'                     { TBNot }
'&'                     { TBAnd }
'|'                     { TBOr }
'^'                     { TBXor }
'('                     { TLParen }
')'                     { TRParen }
'['                     { TLSquare }
']'                     { TRSquare }
','                     { TComma }
"head"                  { THead }
"tail"                  { TTail }
"show"                  { TShow }
"type"                  { TType }
"length"		        { TLength }

%nonassoc "else"
%left '(' ')'
%left '=' "let" "not" '~'
%left "and" "or" "++"
%left '<' '>' ">=" "<=" "==" "!=" ".."
%left '+' '-'
%left '*' '/' "mod" "at" "as" "is"
%left '&' '|' '~' "<<" ">>"
%right "::"
%nonassoc UMINUS

%%

Program : 
Expression                                            { [$1] }
| Expression Program                                  { $1 : $2 }

Expression :
 '(' Expression ')'                                   { $2 }
| integer                                             { PriInteger $1 }
| float                                               { PriDecimal $1 }
| char                                                { PriChar $1 }
| "true"                                              { PriBoolean True }
| "false"                                             { PriBoolean False }
| string                                              { PriString $1 }
| Identifier                                          { $1 }
| "let" Identifier '=' Expression Expression          { PriLet $2 $4 $5 }
| Identifier '=' Expression                           { PriVal $1 $3 }
| "fn" '(' List ')' Expression                        { PriLambda $3 $5 }
| Identifier '(' List ')'                             { PriApply $1 $3 }
| "if" Expression "then" Expression "else" Expression { PriIf $2 $4 $6 }
| Expression "::" Expression                          { PriBinaryOperator Cons $1 $3 }
| Expression '+' Expression                           { PriBinaryOperator Add $1 $3}
| Expression '-' Expression                           { PriBinaryOperator Sub $1 $3}
| Expression '*' Expression                           { PriBinaryOperator Mul $1 $3}
| Expression '/' Expression                           { PriBinaryOperator Div $1 $3}

| Expression '<' Expression                           { PriBinaryOperator Lt $1 $3}
| Expression '>' Expression                           { PriBinaryOperator Gt $1 $3}
| Expression "<=" Expression                          { PriBinaryOperator Lte $1 $3}
| Expression ">=" Expression                          { PriBinaryOperator Gte $1 $3}
| Expression "==" Expression                          { PriBinaryOperator Eq $1 $3}
| Expression "!=" Expression                          { PriBinaryOperator Ne $1 $3}
| Expression "and" Expression                         { PriBinaryOperator And $1 $3}
| Expression "or" Expression                          { PriBinaryOperator Or $1 $3}
| "not" Expression                                    { PriUnaryOperator Not $2 }
| Expression "mod" Expression                         { PriBinaryOperator Mod $1 $3}
| Expression "++" Expression                          { PriBinaryOperator Append $1 $3}
| Expression ".." Expression                          { PriBinaryOperator Range $1 $3}
| "head" '(' Expression ')'                           { PriHead $3 }
| "tail" '(' Expression ')'                           { PriTail $3 }
| "show" '(' Expression ')'                           { PriShow $3 }
| "length" '(' Expression ')'                         { PriLength $3 }
| '-' Expression %prec UMINUS                         { PriUnaryOperator Neg $2 }
| '[' List ']'                                        { PriList $2 }

Identifier :
ident                                                 { PriSymbol $1 }

List :
Expression                                            { [$1] }
| Expression ',' List                                 { $1 : $3 }
|                                                     { [] }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Program = Program Expression
      deriving (Show)

}
