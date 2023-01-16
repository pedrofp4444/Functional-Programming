module Ficha4 where

import Data.Char

-- Exercício 1

digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (h:t)
    | isAlpha h = (  digits, h:letters)
    | isDigit h = (h:digits,   letters)
    | otherwise = (  digits,   letters)
    where (digits, letters) = digitAlpha t

-- Exercício 2

nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t)
    | h < 0 = (n + 1, z, p)
    | h == 0 = (n, z + 1, p)
    | otherwise = (n, z, p + 1)
    where (n, z, p) = nzp t

-- Exercício 3

divMod' :: Integral a => a -> a -> (a, a)
divMod' x y 
    | x < 0 = let (q,r) = divMod' (-x) y in (if r == 0 then (-q,r) else (-q-1,-r+y))
    | y < 0 = let (q,r) = divMod' (-x) (-y) in (q,-r)
    | otherwise = if x - y < 0 then (0,x) else (let (q,r) = divMod' (x - y) y in (q+1,r))

-- Exercício 4

fromDigits :: [Int] -> Int
fromDigits l = fromDigitsAux l 0

fromDigitsAux :: [Int] -> Int -> Int
fromDigitsAux [] acc = acc
fromDigitsAux (h:t) acc = fromDigitsAux t (h + 10 * acc)

-- Exercício 5

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maxSumInitAux l (sum l)

maxSumInitAux :: (Num a, Ord a) => [a] -> a -> a
maxSumInitAux [] acc = acc
maxSumInitAux l acc = if si > acc then maxSumInitAux il si else maxSumInitAux il acc
    where il = init l
          si = sum il

-- Exercício 6

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fibAux (n-2) 0 1

fibAux :: Int -> Int -> Int -> Int
fibAux 0 n _ = n
fibAux i fib_n fib_n_mais_1 = fibAux (i - 1) fib_n_mais_1 (fib_n + fib_n_mais_1)

-- Exercício 7

intToStr :: Integer -> String
intToStr 0 = "zero"
intToStr n = intToStrAux n ""

intToStrAux :: Integer -> String -> String
intToStrAux 0 ('-':acc) = acc
intToStrAux n acc = intToStrAux nn ((case r of 
        0 -> "-zero"
        1 -> "-um"
        2 -> "-dois"
        3 -> "-três"
        4 -> "-quatro"
        5 -> "-cinco"
        6 -> "-seis"
        7 -> "-sete"
        8 -> "-oito"
        9 -> "-nove") ++ acc)
    where (nn,r) = n `divMod` 10

-- Exercícios 8

-- Alínea a)

--[6,12,18]
listaA :: [Integer]
listaA = [x | x <- [1..20], mod x 6 == 0]

-- Alínea b)

--[6,12,18]
listaB :: [Integer]
listaB = [x | x <- [1..20], mod x 6 == 0]

-- Alínea c)

--[(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
listaC :: [(Integer, Integer)]
listaC = [(x, 30 - x) | x <- [10..20]]

-- Alínea d)

--[1,1,4,4,9,9,16,16,25,25]
listaD :: [Integer]
listaD = [ x^2 | x <- [1..5], y <- [1..2]]

-- Exercício 9

-- Alínea a)

listaA' :: [Integer]
listaA' = [ 2^x | x <- [0..10]]

-- Alínea b)

lsitaB' :: [(Integer, Integer)]
lsitaB' = [(x,y) | x <- [1..5], y <- [1..5], x + y == 6]

-- Alínea c)

listaC' :: [[Integer]]
listaC' = [[1..x] | x <- [1..5]]

-- Alínea d)

listaD' :: [[Integer]]
listaD' = [ replicate x 1 | x <- [1..5]]

-- Alínea e)

listaE' :: [Integer]
listaE' = [ product [y | y <- [1..x]] | x <- [1..6]]