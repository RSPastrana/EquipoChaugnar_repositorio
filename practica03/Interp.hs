module Interp where

import Grammars
import Data.List (nub)

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

-- Sustituye x en s por e, evitando captura de variables
-- sust e x s
sust :: ASA -> String -> ASA -> ASA
sust e x (Id y)
  | x == y = e
  | otherwise = Id y
sust _ _ (Num n) = Num n
sust _ _ (Boolean b) = Boolean b
sust e x (And es) = And (map (sust e x) es)
sust e x (Or es) = Or (map (sust e x) es)
sust e x (Add es) = Add (map (sust e x) es)
sust e x (Sub es) = Sub (map (sust e x) es)
sust e x (Mul es) = Mul (map (sust e x) es)
sust e x (Div es) = Div (map (sust e x) es)
sust e x (Lt es) = Lt (map (sust e x) es)
sust e x (Gt es) = Gt (map (sust e x) es)
sust e x (Le es) = Le (map (sust e x) es)
sust e x (Ge es) = Ge (map (sust e x) es)
sust e x (Expt e1 e2) = Expt (sust e x e1) (sust e x e2)
sust e x (EqP e1 e2) = EqP (sust e x e1) (sust e x e2)
sust e x (Not e1) = Not (sust e x e1)
sust e x (Add1 e1) = Add1 (sust e x e1)
sust e x (Sub1 e1) = Sub1 (sust e x e1)
sust e x (ZeroP e1) = ZeroP (sust e x e1)
sust e x (Let bs body)
  | x `elem` boundVars = Let bsSub body
  | otherwise = Let bsFinal (sust e x bodyFinal)
  where
    boundVars = map fst bs
    bsSub = [(y, sust e x rhs) | (y, rhs) <- bs]
    fvE = freeVars e
    initialUsed = fvE ++ names body ++ concatMap (names . snd) bs ++ boundVars ++ [x]
    (bsFinal, bodyFinal, _) = foldl renameStep ([], body, initialUsed) bsSub
    renameStep (accBs, b, used) (y, rhs)
      | y `elem` fvE =
          let z = freshName used
           in (accBs ++ [(z, rhs)], sust (Id z) y b, z : used)
      | otherwise = (accBs ++ [(y, rhs)], b, used)
sust e x (LetStar [] body) = LetStar [] (sust e x body)
sust e x (LetStar ((y, b) : bs) body)
  | y == x = LetStar ((y, sust e x b) : bs) body
  | y `elem` freeVars e =
      let used = freeVars e ++ names (LetStar bs body) ++ [x, y]
          z = freshName used
          LetStar bsR bodyR = sust (Id z) y (LetStar bs body)
          LetStar bsF bodyF = sust e x (LetStar bsR bodyR)
       in LetStar ((z, sust e x b) : bsF) bodyF
  | otherwise =
      let LetStar bsF bodyF = sust e x (LetStar bs body)
       in LetStar ((y, sust e x b) : bsF) bodyF
sustMany :: ASA -> [Binding] -> ASA
sustMany body bs = foldl (\b (p, e) -> sust e p b) afterRename (zip freshs es)
  where
    (xs, es) = unzip bs
    baseUsed = names body ++ concatMap freeVars es ++ xs
    freshs = genFresh baseUsed xs
    genFresh _ [] = []
    genFresh used (_ : rest) =
      let p = freshName used
       in p : genFresh (p : used) rest
    afterRename = foldl (\b (x, p) -> sust (Id p) x b) body (zip xs freshs)
 
-- RETO 4: semantica operacional de paso grande
bigStep :: ASA -> Maybe ASA
bigStep (Num n) = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)
bigStep (Id _) = Nothing
bigStep (And es) = Boolean . and <$> mapM evalBool es
bigStep (Or es) = Boolean . or <$> mapM evalBool es
bigStep (Add es) = Num . sum <$> mapM evalNum es
bigStep (Sub es) = do
  ns <- mapM evalNum es
  case ns of
    [] -> Nothing
    (n : rest) -> Just (Num (foldl monus n rest))
  where
    monus a b = max 0 (a - b)
bigStep (Mul es) = Num . product <$> mapM evalNum es
bigStep (Div es) = do
  ns <- mapM evalNum es
  case ns of
    [] -> Nothing
    (n : rest)
      | 0 `elem` rest -> Nothing
      | otherwise -> Just (Num (foldl div n rest))
bigStep (Lt es) = chainCompare (<) es
bigStep (Gt es) = chainCompare (>) es
bigStep (Le es) = chainCompare (<=) es
bigStep (Ge es) = chainCompare (>=) es
bigStep (Expt e1 e2) = do
  n1 <- evalNum e1
  n2 <- evalNum e2
  Just (Num (n1 ^ n2))
bigStep (EqP e1 e2) = do
  v1 <- bigStep e1
  v2 <- bigStep e2
  case (v1, v2) of
    (Num a, Num b) -> Just (Boolean (a == b))
    (Boolean a, Boolean b) -> Just (Boolean (a == b))
    _ -> Nothing
bigStep (Not e) = do
  v <- bigStep e
  case v of
    Boolean False -> Just (Boolean True)
    _ -> Just (Boolean False)
bigStep (Add1 e) = do
  n <- evalNum e
  Just (Num (n + 1))
bigStep (Sub1 e) = do
  n <- evalNum e
  Just (Num (max 0 (n - 1)))
bigStep (ZeroP e) = do
  v <- bigStep e
  case v of
    Num n -> Just (Boolean (n == 0))
    _ -> Nothing
bigStep (Let bs body)
  | length (nub (map fst bs)) /= length bs = Nothing
  | otherwise = do
      vals <- mapM (bigStep . snd) bs
      bigStep (sustMany body (zip (map fst bs) vals))
bigStep (LetStar [] body) = bigStep body
bigStep (LetStar ((x, e) : bs) body) = do
  v <- bigStep e
  bigStep (sust v x (LetStar bs body))
 
evalNum :: ASA -> Maybe Int
evalNum e = case bigStep e of
  Just (Num n) -> Just n
  _ -> Nothing
 
evalBool :: ASA -> Maybe Bool
evalBool e = case bigStep e of
  Just (Boolean b) -> Just b
  _ -> Nothing
 
chainCompare :: (Int -> Int -> Bool) -> [ASA] -> Maybe ASA
chainCompare op es = do
  ns <- mapM evalNum es
  Just (Boolean (and (zipWith op ns (tail ns))))
