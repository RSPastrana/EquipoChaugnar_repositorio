module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]
freeVars (Id x) = [x]
freeVars (Num _) = []
freeVars (Boolean _) = []
freeVars (And es) = concatMap freeVars es
freeVars (Or es) = concatMap freeVars es
freeVars (Add es) = concatMap freeVars es
freeVars (Sub es) = concatMap freeVars es
freeVars (Mul es) = concatMap freeVars es
freeVars (Div es) = concatMap freeVars es
freeVars (Lt es) = concatMap freeVars es
freeVars (Gt es) = concatMap freeVars es
freeVars (Le es) = concatMap freeVars es
freeVars (Ge es) = concatMap freeVars es
freeVars (Expt e1 e2) = freeVars e1 ++ freeVars e2
freeVars (EqP e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Not e) = freeVars e
freeVars (Add1 e) = freeVars e
freeVars (Sub1 e) = freeVars e
freeVars (ZeroP e) = freeVars e
freeVars (Let bs body) =
  concatMap (freeVars . snd) bs
    ++ filter (`notElem` map fst bs) (freeVars body)
freeVars (LetStar [] body) = freeVars body
freeVars (LetStar ((x, e) : bs) body) =
  freeVars e ++ filter (/= x) (freeVars (LetStar bs body))



names :: ASA -> [String]
names (Id x) = [x]
names (Num _) = []
names (Boolean _) = []
names (And es) = concatMap names es
names (Or es) = concatMap names es
names (Add es) = concatMap names es
names (Sub es) = concatMap names es
names (Mul es) = concatMap names es
names (Div es) = concatMap names es
names (Lt es) = concatMap names es
names (Gt es) = concatMap names es
names (Le es) = concatMap names es
names (Ge es) = concatMap names es
names (Expt e1 e2) = names e1 ++ names e2
names (EqP e1 e2) = names e1 ++ names e2
names (Not e) = names e
names (Add1 e) = names e
names (Sub1 e) = names e
names (ZeroP e) = names e
names (Let bs body) =
  map fst bs ++ concatMap (names . snd) bs ++ names body
names (LetStar bs body) =
  map fst bs ++ concatMap (names . snd) bs ++ names body

--se filtra la lista de nombres para obtener un nombre nuevo, candidates es una lista infinita de nombres posibles, x1, x2, x3, ... y head filtra el primer elemento que no este en la lista xs
freshName :: [String] -> String
freshName xs = head $ filter (`notElem` xs) candidates
  where
    candidates = [ "x" ++ show n | n <- [1 ..] ]

sust :: ASA -> String -> ASA -> ASA

sustMany :: ASA -> [Binding] -> ASA
sustMany _ = error "sustMany: no completado"

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
bigStep _ = error "bigStep: no completado"