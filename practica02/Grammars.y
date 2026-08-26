{
module Grammars where

import Lexer (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      nat             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMul }
      '/'             { TokenDiv }
      "and"           { TokenAnd }
      "or"            { TokenOr }
      "not"           { TokenNot }
      "add1"          { TokenAdd1 }
      "sub1"          { TokenSub1 }
      "zero?"         { TokenZeroP }
      "expt"          { TokenExpt }
      '<'             { TokenLT }
      '>'             { TokenGT }
      "<="            { TokenLE }
      ">="            { TokenGE }
      "eq"            { TokenEq }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

ASA : nat                      { Num $1 }
    | bool                     { Boolean $1 }

-- RETO 2:
-- Agrega las producciones para:
--   * operadores n-arios con al menos dos argumentos;
--   * operadores estrictamente binarios: expt y eq;
--   * operadores unarios: not, add1, sub1, zero?.
    | '(' '+' ASA ASA ')'             { Add [$1, $3] }
    | '(' '-' ASA ASA ')'             { Sub [$1, $3] }
    | '(' '*' ASA ASA ')'             { Mul [$1, $3] }
    | '(' '/' ASA ASA ')'             { Div [$1, $3] }
    | '(' "and" ASA ASA ')'           { And [$1, $3] }
    | '(' "or" ASA ASA ')'            { Or [$1, $3] }
    | '(' "<" ASA ASA ')'             { Lt [$1, $3] }
    | '(' ">" ASA ASA ')'             { Gt [$1, $3] }
    | '(' "<=" ASA ASA ')'            { Le [$1, $3] }
    | '(' ">=" ASA ASA ')'            { Ge [$1, $3] }
    | '(' "expt" ASA ASA ')'          { Expt $1 $3 }
    | '(' "eq" ASA ASA ')'            { EqP $1 $3 }
    | '(' "not" ASA ')'               { Not $2 }
    | '(' "add1" ASA ')'              { Add1 $2 }
    | '(' "sub1" ASA ')'              { Sub1 $2 }
    | '(' "zero?" ASA ')'             { ZeroP $2 }


-- RETO 3:
-- Agrega un no terminal para representar dos o mas argumentos.
-- El resultado debe ser una lista de ASA.

add : ASA '+' ASA             { [$1, $3] }
    | add '+' ASA             { $1 ++ [$3] }

sub : ASA '-' ASA             { [$1, $3] }
    | sub '-' ASA             { $1 ++ [$3] }

mul : ASA '*' ASA             { [$1, $3] }
    | mul '*' ASA             { $1 ++ [$3] }

div : ASA '/' ASA             { [$1, $3] }
    | div '/' ASA             { $1 ++ [$3] }

and : ASA "and" ASA           { [$1, $3] }
    | and "and" ASA           { $1 ++ [$3] }

or : ASA "or" ASA             { [$1, $3] }
    | or "or" ASA             { $1 ++ [$3] }

lt : ASA '<' ASA             { [$1, $3] }
    | lt '<' ASA              { $1 ++ [$3] }

gt : ASA '>' ASA             { [$1, $3] }
    | gt '>' ASA              { $1 ++ [$3] }

le : ASA "<=" ASA            { [$1, $3] }
    | le "<=" ASA             { $1 ++ [$3] }

ge : ASA ">=" ASA            { [$1, $3] }
    | ge ">=" ASA             { $1 ++ [$3] }

expt : ASA "expt" ASA          { [$1, $3] }

eq : ASA "eq" ASA            { [$1, $3] }

not : "not" ASA               { [$2] }

add1 : "add1" ASA              { [$2] }

sub1 : "sub1" ASA              { [$2] }

zero? : "zero?" ASA             { [$2] }

{
parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

data ASA
  = Num Int
  | Boolean Bool
  | And [ASA]
  | Or [ASA]
  | Add [ASA]
  | Sub [ASA]
  | Mul [ASA]
  | Div [ASA]
  | Lt [ASA]
  | Gt [ASA]
  | Le [ASA]
  | Ge [ASA]
  | Expt ASA ASA
  | EqP ASA ASA
  | Not ASA
  | Add1 ASA
  | Sub1 ASA
  | ZeroP ASA
  deriving (Eq, Show)
}
