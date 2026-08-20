--
--Equipo: Chaugnar
--Integrantes:
-- 1. Camila Hernández Huchin
-- 2. Rafael Sánchez Pastrana
--

module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen n m = sqrt (n * n + m * m)


sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (map (^ 2) (filter even xs))

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

varianza2 :: Double -> Double -> Double
varianza2 n m =
    let media = (n + m) / 2
     in ((n - media) ^2 + (m - media) ^ 2 )/ 2

clasificaTemperatura :: Int -> String 
clasificaTemperatura n 
            | n <= 0 = "frio extremo"
            | n <= 15 = "frio"
            | n <= 25 = "templado"
            | n <= 35 = "calido"
            | otherwise = "calor extremo"

intercala :: a -> [a] -> [a]
intercala x [] = []
intercala x [y] = [y]
intercala x (y:ys) = y : x : intercala x ys

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma e1 e2) = evalua e1 + evalua e2
evalua (Producto e1 e2) = evalua e1 * evalua e2
