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
"length"                { TLength }
"as"                    { TAs }
"is"                    { TIs }

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
| integer                                             { PriInt $1 }
| float                                               { PriFloat $1 }
| char                                                { PriChar $1 }
| "true"                                              { PriBool True }
| "false"                                             { PriBool False }
| string                                              { PriString $1 }
| Identifier                                          { $1 }
| "let" Identifier '=' Expression Expression          { PriLet $2 $4 $5 }
| Identifier '=' Expression                           { PriVal $1 $3 }
| "fn" '(' List ')' Expression                        { PriLambda $3 $5 }
| Identifier '(' List ')'                             { PriApply $1 $3 }
| "if" Expression "then" Expression "else" Expression { PriIf $2 $4 $6 }
| Expression "::" Expression                          { PriBinop Cons $1 $3 }
| Expression '+' Expression                           { PriBinop Add $1 $3}
| Expression '-' Expression                           { PriBinop Sub $1 $3}
| Expression '*' Expression                           { PriBinop Mul $1 $3}
| Expression '/' Expression                           { PriBinop Div $1 $3}

| Expression '<' Expression                           { PriBinop Lt $1 $3}
| Expression '>' Expression                           { PriBinop Gt $1 $3}
| Expression "<=" Expression                          { PriBinop Lte $1 $3}
| Expression ">=" Expression                          { PriBinop Gte $1 $3}
| Expression "==" Expression                          { PriBinop Eq $1 $3}
| Expression "!=" Expression                          { PriBinop Ne $1 $3}
| Expression "and" Expression                         { PriBinop And $1 $3}
| Expression "or" Expression                          { PriBinop Or $1 $3}
| "not" Expression                                    { PriUniop Not $2 }
| Expression "mod" Expression                         { PriBinop Mod $1 $3}
| Expression "++" Expression                          { PriBinop App $1 $3}
| Expression ".." Expression                          { PriBinop Rge $1 $3}
| "head" '(' Expression ')'                           { PriHead $3 }
| "tail" '(' Expression ')'                           { PriTail $3 }
| "show" '(' Expression ')'                           { PriShow $3 }
| "length" '(' Expression ')'                         { PriLength $3 }
| '-' Expression %prec UMINUS                         { PriUniop Neg $2 }
| '[' List ']'                                        { PriList $2 }
| Expression "as" Expression                          { PriCast $1 $3 }
| Expression "is" Expression                          { PriIs $1 $3 }

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
