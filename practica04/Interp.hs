module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e
  | x `elem` xs = Nothing
  | otherwise = do
      e' <- curryFun xs e
      return (Fun x e')

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp f [e] = Just (App f e)
curryApp f es = Just (foldl App f es)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [e] = Nothing
binaryOp op (e1:e2:es) = Just (foldl op (op e1 e2) es)

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (IdS x) = Just (Id x)
desugar (AddS es) = do
  es' <- mapM desugar es
  binaryOp Add es'
desugar (SubS es) = do
  es' <- mapM desugar es
  binaryOp Sub es'
desugar (NotS e) = do
  e' <- desugar e
  return (Not e')
desugar (FunS xs e) = do
  e' <- desugar e
  curryFun xs e'
desugar (AppS f es) = do
  f' <- desugar f
  es' <- mapM desugar es
  curryApp f' es'
desugar (LetS x e1 e2) = do
  e1' <- desugar e1
  e2' <- desugar e2
  return (App (Fun x e2') e1')
desugar (LetStarS [] e) = desugar e
desugar (LetStarS ((x, e1):xs) e2) = do
  e1' <- desugar e1
  e2' <- desugar (LetStarS xs e2)
  return (App (Fun x e2') e1')

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv x [] = Nothing
lookupEnv x ((y, v):ys)
  | x == y = Just v
  | otherwise = lookupEnv x ys


-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep env e = case e of
  Id y -> lookupEnv y env
  Num a -> Just (NumV a)
  Boolean b -> Just (BooleanV b)
  Add e1 e2 -> do
    v1 <- bigStep env e1  
    v2 <- bigStep env e2
    case (v1, v2) of
      (NumV n1, NumV n2) -> Just (NumV (n1 + n2))
      _ -> Nothing
  Sub e1 e2 -> do
    v1 <- bigStep env e1
    v2 <- bigStep env e2
    case (v1, v2) of
      (NumV n1, NumV n2) -> Just (NumV (max 0 (n1 - n2)))
      _ -> Nothing
  Not e1 -> do
    v1 <- bigStep env e1
    case v1 of
      BooleanV b -> Just (BooleanV (not b))
      --todo numero cuenta como verdadero cuando aparece como operando de Not.
      NumV n -> Just (BooleanV False)
      _ -> Nothing
  Fun y e1 -> Just (ClosureV y e1 env)  
  App ef ea -> do
    v1 <- bigStep env ef
    case v1 of
      ClosureV y body envC -> do
        v2 <- bigStep env ea
        bigStep ((y, v2) : envC) body
      _ -> Nothing
          