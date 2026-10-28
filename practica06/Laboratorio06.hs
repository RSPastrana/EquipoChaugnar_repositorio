module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola = undefined

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS = undefined

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
reversaCola = undefined

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS = undefined

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
intercalaCola = undefined

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS = undefined