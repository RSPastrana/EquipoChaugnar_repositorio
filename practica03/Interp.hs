module Interp where

import Data.List (nub)
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

freshName :: [String] -> String
freshName xs = head $ filter (`notElem` xs) candidates
  where
    candidates = ["x" ++ show n | n <- [1 ..]]

sust :: ASA -> String -> ASA -> ASA
sust (Id y) x e
  | x == y = e
  | otherwise = Id y
sust (Num n) _ _ = Num n
sust (Boolean b) _ _ = Boolean b
sust (And ss) x e = And (map (\s -> sust s x e) ss)
sust (Or ss) x e = Or (map (\s -> sust s x e) ss)
sust (Add ss) x e = Add (map (\s -> sust s x e) ss)
sust (Sub ss) x e = Sub (map (\s -> sust s x e) ss)
sust (Mul ss) x e = Mul (map (\s -> sust s x e) ss)
sust (Div ss) x e = Div (map (\s -> sust s x e) ss)
sust (Lt ss) x e = Lt (map (\s -> sust s x e) ss)
sust (Gt ss) x e = Gt (map (\s -> sust s x e) ss)
sust (Le ss) x e = Le (map (\s -> sust s x e) ss)
sust (Ge ss) x e = Ge (map (\s -> sust s x e) ss)
sust (Expt s1 s2) x e = Expt (sust s1 x e) (sust s2 x e)
sust (EqP s1 s2) x e = EqP (sust s1 x e) (sust s2 x e)
sust (Not s1) x e = Not (sust s1 x e)
sust (Add1 s1) x e = Add1 (sust s1 x e)
sust (Sub1 s1) x e = Sub1 (sust s1 x e)
sust (ZeroP s1) x e = ZeroP (sust s1 x e)
sust (Let bs body) x e
  | x `elem` boundVars = Let bsSub body
  | otherwise = Let bsFinal (sust bodyFinal x e)
  where
    boundVars = map fst bs
    bsSub = [(y, sust rhs x e) | (y, rhs) <- bs]
    fvE = freeVars e
    initialUsed = fvE ++ names body ++ concatMap (names . snd) bs ++ boundVars ++ [x]
    (bsFinal, bodyFinal, _) = foldl renameStep ([], body, initialUsed) bsSub
    renameStep (accBs, b, used) (y, rhs)
      | y `elem` fvE =
          let z = freshName used
           in (accBs ++ [(z, rhs)], sust b y (Id z), z : used)
      | otherwise = (accBs ++ [(y, rhs)], b, used)
sust (LetStar [] body) x e = LetStar [] (sust body x e)
sust (LetStar ((y, b) : bs) body) x e
  | y == x = LetStar ((y, sust b x e) : bs) body
  | y `elem` freeVars e =
      let used = freeVars e ++ names (LetStar bs body) ++ [x, y]
          z = freshName used
          LetStar bsR bodyR = sust (LetStar bs body) y (Id z)
          LetStar bsF bodyF = sust (LetStar bsR bodyR) x e
       in LetStar ((z, sust b x e) : bsF) bodyF
  | otherwise =
      let LetStar bsF bodyF = sust (LetStar bs body) x e
       in LetStar ((y, sust b x e) : bsF) bodyF

sustMany :: ASA -> [Binding] -> ASA
sustMany body bs = foldl (\b (p, val) -> sust b p val) afterRename (zip freshs es)
  where
    (xs, es) = unzip bs
    baseUsed = names body ++ concatMap freeVars es ++ xs
    freshs = genFresh baseUsed xs
    genFresh _ [] = []
    genFresh used (_ : rest) =
      let p = freshName used
       in p : genFresh (p : used) rest
    afterRename = foldl (\b (x, p) -> sust b x (Id p)) body (zip xs freshs)

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
  bigStep (sust (LetStar bs body) x v)

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