module Questoes where

-- Exercício 1

enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 a b 
            | a < b = a : enumFromTo1 (a+1) b 
            | a > b = a : enumFromTo1 (a-1) b   
            | a == b = [a]

-- Exercício 2

enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 a b c
                | a <= b && a <= c = a : enumFromThenTo1 b (b+(b-a)) c 
                | a >= b && a >= c = a : enumFromThenTo1 b (b-(a-b)) c 
                | otherwise = []

-- Exercício 3

concatenar :: [a] -> [a] -> [a]
concatenar [] x = x
concatenar (h:t) l = h : concatenar t l

-- Exercício 4

localizarelem :: [a] -> Int -> a
localizarelem (h:t) b
    | b == 0 = h
    | b /= 0 = localizarelem t (b-1)

excla :: [a] -> Int -> a 
excla l x 
    | x == 0 = head l
    | x > 0 = excla (tail l) (x-1)

-- Exercício 5

reverse1 :: [a] -> [a]
reverse1 l
        | length l == 0 = l
        | otherwise = last l : reverse1 (init l)

-- Exercício 6

takep :: Int -> [a] -> [a]
takep a (h:t)
    | a == 0 = []
takep a (h:t) = h : takep (a-1) t

-- Exercício 7 (ATENÇÃO)

{-Quando o a é negativo devolve a lista segundo a função pré-definida-}

drop1 :: Int -> [a] -> [a]
drop1 a [] = [] 
drop1 a (h:t)
        | a <= 0 = (h:t)
        | a > 0 = drop1 (a-1) t

-- Exercício 8

zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (h:t) = (x,h) : zip1 xs t 

-- Exercício 9

replicate1 :: Int -> a -> [a]
replicate1 b a 
        | b <= 0 = []
        | otherwise = a : replicate1 (b-1) a

-- Exercício 10

intersperse1 :: a -> [a] -> [a]
intersperse1 _ [] = []
intersperse1 _ [x] = [x]
intersperse1 a (h:t) = h : a : intersperse1 a t

-- Exercício 11

group1 :: Eq a => [a] -> [[a]]
group1 [] = [[]]
group1 (h:t) = group2 [h] t
        where
        group2 :: Eq a => [a] -> [a] -> [[a]]
        group2 l [] = [l]
        group2 l (h:t) | elem h l = group2 (h:l) t
                       | otherwise = l : group2 [h] t

-- Exercício 12 

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (h:t) = h ++ concat1 t

-- Exercício 13

inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 l = inits1 (init l) ++ [l] 

-- Exercício 14

tails1 :: [a] -> [[a]]
tails1 [] = []
tails1 (h:[]) = [[h],[]]
tails1 (h:t) = [(h:t)] ++ tails1 t

-- Exercício 15

heads1 :: [[a]] -> [a]
heads1 [] = []
heads1 ([]:t) = heads1 t 
heads1 (h:t) = head h : heads1 t

-- Exercício 16

total1 :: [[a]] -> Int
total1 [] = 0
total1 (h:t) = length h + total1 t

-- Exercício 17 (ATENÇÃO)

{-Caso de paragem estava errado-}

fun1 :: [(a,b,c)] -> [(a,c)]
fun1 [] = []
fun1 ((a,b,c):t) = (a,c) : fun1 t

-- Exercício 18 

cola1 :: [(String,b,c)] -> String
cola1 [] = []
cola1 ((a,b,c):t) = a ++ cola1 t 

-- Exercício 19 

idade1 :: Int -> Int -> [(String,Int)] -> [String]
idade1 _ _ [] = []
idade1 a i ((n,b):t) = if (a - b) >= i
                       then n : idade1 a i t
                       else idade1 a i t

-- Exercício 20

powerEnumFrom1 :: Int -> Int -> [Int]
powerEnumFrom1 n m
    | m >= 1 = powerEnumFrom1 n (m - 1) ++ [n^(m-1)]
    | otherwise = []

-- Exercício 21

isPrime1 :: Int -> Bool
isPrime1 n = if n >= 2 
             then isPrime2 n 2
             else False
        where  
                isPrime2 :: Int -> Int -> Bool
                isPrime2 n m 
                          | m^2 > n = True -- m > sqrt n
                          | mod n m == 0 = False
                          | otherwise = isPrime2 n (m + 1)

-- Exercício 22

isPrefixOf1 :: Eq a => [a] -> [a] -> Bool
isPrefixOf1 [] _ = True
isPrefixOf1 _ [] = False
isPrefixOf1 (h1:t1) (h2:t2) = h1 == h2 && isPrefixOf1 t1 t2

-- Exercício 23 (ATENÇÃO)

{-A definição do caso de paragem implica que lista vazia é sempre sufixo-}

isSuffixOf1 :: Eq a => [a] -> [a] -> Bool
isSuffixOf1 [] _ = True
isSuffixOf1 _ [] = False
isSuffixOf1 l (h:t) = l == (h:t) || isSuffixOf1 l t

-- Exercício 24

isSubsequenceOf1 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf1 [] _ = True
isSubsequenceOf1 _ [] = False
isSubsequenceOf1 (h1:t1) (h2:t2) = (h1 == h2 && isSubsequenceOf1 t1 t2) || isSubsequenceOf1 (h1:t1) t2

-- Exercício 25 (ATENÇÃO)

elemIndices1 :: Eq a => a -> [a] -> [Int]
elemIndices1 a b = auxiliar a b 0
        where
           auxiliar :: Eq a => a -> [a] -> Int -> [Int]
           auxiliar _ [] _ = []
           auxiliar x (h:t) y
                           | x == h = y : auxiliar x t (y+1)
                           | x /= h = auxiliar x t (y+1)

-- Exercício 26 (ATENÇÃO)

{-Não obtém a ordem correspondente ao enunciado-}

nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (h:t) 
        | elem h t = nub1 t
        | otherwise = h : nub1 t

-- Exercício 27

delete1 :: Eq a => a -> [a] -> [a]
delete1 _ [] = []
delete1 a (h:t) = if a == h 
                  then t 
                  else h : delete1 a t 

-- Exercício 28

tirar :: Eq a => [a] -> [a] -> [a]
tirar [] _ = []
tirar l [] = l
tirar l (h:t) = tirar (apagar h l) t
        where 
                apagar :: Eq a => a -> [a] -> [a]
                apagar _ [] = []
                apagar a (h:t) 
                        | a == h = t
                        | otherwise = h : apagar a t

-- Exercício 29

union1 :: Eq a => [a] -> [a] -> [a]
union1 l (h:t)
        | elem h l = union1 l t
        | otherwise = union1 (l ++ [h]) t

-- Exercício 30

intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 [] _ = []
intersect1 (h:t) l
    | elem h l = h : intersect1 t l
    | otherwise = intersect1 t l

-- Exercício 31

insert1 :: Ord a => a -> [a] -> [a]
insert1 a [] = [a]
insert1 a (h:t)
    | a > h = h : insert1 a t
    | otherwise = a : h : t

-- Exercício 32

unwords1 :: [String] -> String
unwords1 [] = ""
unwords1 (h:t) = h ++ (if null t then "" else " ") ++ unwords1 t

-- Exercício 33

unlines1 :: [String] -> String
unlines1 [] = ""
unlines1 (h:t) = h ++ "\n" ++ unlines1 t

-- Exercício 34

pMaior1 :: Ord a => [a] -> Int
pMaior1 [_] = 0
pMaior1 (h:t) = let x = pMaior1 t in
                if h >= (t !! x)
                then 0
                else x + 1

-- Exercício 35

lookup1 :: Eq a => a -> [(a,b)] -> Maybe b
lookup1 _ [] = Nothing
lookup1 x ((a,b):t) = if x == a
                     then Just b
                     else lookup1 x t

-- Exercício 36

preCrescente1 :: Ord a => [a] -> [a]
preCrescente1 [] = []
preCrescente1 [a] = [a]
preCrescente1 (h1:h2:t) = if h2 >= h1
                         then h1 : preCrescente1 (h2:t)
                         else [h1]

-- Exercício 37 

iSort1 :: Ord a => [a] -> [a]
iSort1 [] = []
iSort1 (h:t) = auxiliar h (iSort1 t)
        where
           auxiliar :: Ord a => a -> [a] -> [a]
           auxiliar a [] = [a]
           auxiliar a (h:t)
               | a > h = h : auxiliar a t
               | otherwise = a : h : t

-- Exercício 38

menor1 :: String -> String -> Bool
menor1 _ [] = False
menor1 [] _ = True
menor1 (h1:t1) (h2:t2) = if h1 < h2
                        then True
                        else if h1 == h2 
                             then menor1 t1 t2
                             else False

-- Exercício 39 

elemMSet1 ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet1 _ [] = False
elemMSet1 a ((h,_):t) = a == h || elemMSet1 a t

-- Exercício 40

converteMSet1 :: [(a,Int)] -> [a]
converteMSet1 [] = []
converteMSet1 ((h1,1):t) = h1 : converteMSet1 t
converteMSet1 ((h1,h2):t) = h1 : converteMSet1 ((h1,h2-1):t)

-- Exercício 41

insereMSet1 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet1 a [] = [(a,1)]
insereMSet1 a ((b,c):t) = if a == b 
                         then (b,c + 1) : t 
                         else (b,c) : insereMSet1 a t

-- Exercício 42

removeMSet1 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet1 a [] = []
removeMSet1 a ((b,c):t) = if a == b
                          then if c > 1
                               then (b, c-1) : t
                               else t 
                          else (b,c) : removeMSet1 a t      

-- Exercício 43

constroiMSet1 :: Ord a => [a] -> [(a,Int)]
constroiMSet1 [] = []
constroiMSet1 l = auxiliar (last l) (constroiMSet1 (init l))
        where -- Esta função auxiliar é igual ao insereMSet1
          auxiliar :: Eq a => a -> [(a,Int)] -> [(a,Int)]
          auxiliar a [] = [(a,1)]
          auxiliar a ((b,c):t) = if a == b 
                                   then (b,c + 1) : t 
                                   else (b,c) : auxiliar a t

-- Exercício 44

partitionEithers1 :: [Either a b] -> ([a],[b])
partitionEithers1 [] = ([],[])
partitionEithers1 ((Left a):t) = (a : at,bt)
    where (at,bt) = partitionEithers1 t
partitionEithers1 ((Right b):t) = (at,b : bt)
    where (at,bt) = partitionEithers1 t

-- Exercício 45

catMaybes1 :: [Maybe a] -> [a]
catMaybes1 [] = []
catMaybes1 (h:t) = case h of Nothing -> catMaybes1 t
                             Just a -> a : catMaybes1 t

-- Exercício 46

data Movimento = Norte | Sul | Este | Oeste deriving Show

caminho1 :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho1 (xi, yi) (xf, yf) 
    | yi < yf = Norte : caminho1 (xi, yi + 1) (xf, yf)
    | yi > yf = Sul : caminho1 (xi, yi - 1) (xf, yf)
    | xi < xf = Este : caminho1 (xi + 1, yi) (xf, yf)
    | xi > xf = Oeste : caminho1 (xi - 1, yi) (xf, yf)
    | otherwise = []

-- Exercício 47 

--data Movimento = Norte | Sul | Este | Oeste deriving Show

hasLoops1 :: (Int,Int) -> [Movimento] -> Bool
hasLoops1 _ [] = False
hasLoops1 i t = (i == pos i t) || (hasLoops1 i (init t))
        where 
                pos :: (Int,Int) -> [Movimento] -> (Int,Int)
                pos l [] = l
                pos (x, y) l@(h:t) = pos a t
                        where a = case h of {Norte -> (x, y + 1); Sul -> (x, y - 1); Este -> (x + 1, y); Oeste -> (x - 1, y)}

-- Exercício 50

data Equipamento = Bom | Razoavel | Avariado deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Bom:t) = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (_:t) = naoReparar t