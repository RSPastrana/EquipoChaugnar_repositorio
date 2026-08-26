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
    | ASA '+' ASA             { Add [$1, $3] }
    | ASA '-' ASA             { Sub [$1, $3] }
    | ASA '*' ASA             { Mul [$1, $3] }
    | ASA '/' ASA             { Div [$1, $3] }
    | ASA "and" ASA           { And [$1, $3] }
    | ASA "or" ASA            { Or [$1, $3] }
    | ASA "<" ASA             { Lt [$1, $3] }
    | ASA ">" ASA             { Gt [$1, $3] }
    | ASA "<=" ASA            { Le [$1, $3] }
    | ASA ">=" ASA            { Ge [$1, $3] }
    | ASA "expt" ASA          { Expt $1 $3 }
    | ASA "eq" ASA            { EqP $1 $3 }
    | "not" ASA               { Not $2 }
    | "add1" ASA              { Add1 $2 }
    | "sub1" ASA              { Sub1 $2 }
    | "zero?" ASA             { ZeroP $2 }


-- RETO 3:
-- Agrega un no terminal para representar dos o mas argumentos.
-- El resultado debe ser una lista de ASA.



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
