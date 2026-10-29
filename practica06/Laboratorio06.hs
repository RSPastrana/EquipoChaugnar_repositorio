module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola base exponente = potenciaColaAux base exponente 1

potenciaColaAux ::  Int -> Int -> Int -> Int 
potenciaColaAux _ 0 acc = acc
potenciaColaAux base exponente acc = potenciaColaAux base (exponente-1) (acc*base)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1
potenciaCPS base exponente k = potenciaCPS base (exponente-1) (\r -> k (r*base))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = longitudColaAux xs 0

longitudColaAux :: [a] -> Int -> Int
longitudColaAux [] acc = acc
longitudColaAux (x : xs) acc = longitudColaAux xs (acc + 1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (x : xs) k = longitudCPS xs (\v -> k (1 + v))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola lista = reversaColaAux lista []

reversaColaAux :: [a] -> [a] -> [a]
reversaColaAux [] acc = acc
reversaColaAux (x:xs) acc = reversaColaAux xs ([x]++acc)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x:xs) k = reversaCPS xs (\r -> k (r++[x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xs = concatenaColaAux xs []

concatenaColaAux :: [[a]] -> [a] -> [a]
concatenaColaAux [] acc = acc
concatenaColaAux (x : xs) acc = concatenaColaAux xs (acc ++ x)

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (x : xs) k = concatenaCPS xs (\ys -> k (x ++ ys))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys  = intercalaColaAux xs ys [] 

intercalaColaAux :: [a] -> [a] -> [a] -> [a]
intercalaColaAux [] ys acc = acc++ys
intercalaColaAux xs [] acc = acc++xs
intercalaColaAux (x:xs) (y:ys) acc = intercalaColaAux xs ys (acc++[x]++[y])


intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x:xs) (y:ys) k = intercalaCPS xs ys (\r -> k (x:y:r))