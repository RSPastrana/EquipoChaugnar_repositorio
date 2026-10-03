module Interp where

import Grammars
import Data.String (String)

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e
  | x `elem` xs = Nothing
  | otherwise   = maybe Nothing (\e' -> Just (Fun x e')) (curryFun xs e)

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp f [e] = Just (App f e)
curryApp f es = Just (foldl App f es)

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [e] = Nothing
binaryOp op (e1:e2:es) = Just (foldl op (op e1 e2) es)

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] e = desugar e
desugarCond ((c, t):cs) e =
  desugar c >>= \c' ->
  desugar t >>= \t' ->
  fmap (If c' t') (desugarCond cs e)

--al parecer no se usa let por que no identifica ni expande los maybe, asi que por eso se usa >>= para prpagarlo y que de JUST  :o

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (NumS n)        = Just (Num n)
desugar (BooleanS b)    = Just (Boolean b)
desugar (IdS x)         = Just (Id x)
desugar (AddS es)       = binaryOp Add =<< mapM desugar es
desugar (SubS es)       = binaryOp Sub =<< mapM desugar es
desugar (NotS e)        = fmap Not (desugar e)
desugar (FunS xs e)     = curryFun xs =<< desugar e
desugar (AppS f es)     =
  desugar f >>= \f' ->
  curryApp f' =<< mapM desugar es
desugar (LetS x e1 e2)  = mkLet x (desugar e1) (desugar e2)
desugar (LetStarS [] e) = desugar e
desugar (LetStarS ((x, e1):xs) e2) = mkLet x (desugar e1) (desugar (LetStarS xs e2))
desugar (CondS cs e)    = desugarCond cs e
desugar (LetRecS f def cuerpo) =
  desugar (LetS f (AppS (IdS "Y") [FunS [f] def]) cuerpo)
desugar (IfS c t e) =
  desugar c >>= \c' ->
  desugar t >>= \t' ->
  fmap (If c' t') (desugar e)

mkLet :: Nombre -> Maybe ASA -> Maybe ASA -> Maybe ASA
mkLet x m1 m2 = m1 >>= \e1' -> fmap (\e2' -> App (Fun x e2') e1') m2


-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv x [] = Nothing
lookupEnv x ((y, v):ys)
  | x == y = Just v
  | otherwise = lookupEnv x ys

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (NumV n)              = Just (NumV n)
strict (BooleanV b)          = Just (BooleanV b)
strict (ClosureV y body env) = Just (ClosureV y body env)
strict (ExprV e env)         = bigStep env e >>= strict

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x)       = lookupEnv x env
bigStep env (Num n)      = Just (NumV n)
bigStep env (Boolean b)  = Just (BooleanV b)
bigStep env (Fun x body) = Just (ClosureV x body env)

bigStep env (Add e1 e2) =
  bigStep env e1 >>= strict >>= asNum >>= \n1 ->
  bigStep env e2 >>= strict >>= asNum >>= \n2 ->
  Just (NumV (n1 + n2))

bigStep env (Sub e1 e2) =
  bigStep env e1 >>= strict >>= asNum >>= \n1 ->
  bigStep env e2 >>= strict >>= asNum >>= \n2 ->
  Just (NumV (max 0 (n1 - n2)))

bigStep env (Not e) =
  bigStep env e >>= strict >>= asBool >>= \b ->
  Just (BooleanV (not b))

bigStep env (If c t e) =
  bigStep env c >>= strict >>= asBool >>= \b ->
  if b then bigStep env t else bigStep env e

bigStep env (App f a) =
  bigStep env f >>= strict >>= \v ->
  applyClosure v a env

applyClosure :: Value -> ASA -> Env -> Maybe Value
applyClosure (ClosureV x body defEnv) arg callEnv =
  bigStep ((x, ExprV arg callEnv) : defEnv) body
applyClosure _ _ _ = Nothing

asNum :: Value -> Maybe Int
asNum (NumV n) = Just n
asNum _        = Nothing

asBool :: Value -> Maybe Bool
asBool (BooleanV b) = Just b
asBool (NumV _)     = Just True
asBool (ClosureV _ _ _) = Just True
asBool _            = Nothing
--todo numero cuenta como verdadero cuando aparece como operando de Not.