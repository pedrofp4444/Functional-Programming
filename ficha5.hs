module Ficha5 where

import Data.List

-- Exercício 1

-- Alínea a)

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = f h || any' f t

-- Alínea b)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
zipWith' _ _ _ = []

-- Alínea c)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (h:t) 
    | f h = h : takeWhile' f t
    | otherwise = []

-- Aínea d)

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (h:t) 
    | f h = dropWhile' f t
    | otherwise = h:t

-- Alínea e)

span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (h:t) 
    | f h = let (taken, dropped) = span' f t in (h:taken, dropped)             
    | otherwise = ([], h:t)

-- Alínea f)

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f x (h:t) 
    | f x h = t
    | otherwise = h : deleteBy' f x t

-- Alínea g)

sortOn' :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (h:t) = insertOn f h (sortOn' f t)
    
insertOn :: (Ord b) => (a -> b) -> a -> [a] -> [a]
insertOn _ x [] = [x]
insertOn f x (h:t) = if f x > f h then h : insertOn f x t else x : h : t

-- Exercício 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- Alínea a)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\x -> snd x == n) p 

-- Alínea b)

conta :: Int -> Polinomio -> Int
conta n p = length $ selgrau n p

-- Alínea c)

grau :: Polinomio -> Int
grau p = maximum $ map snd p

-- Alínea d)

deriv :: Polinomio -> Polinomio
deriv p = map (\(c,g) -> (c * fromIntegral g, g - 1)) $ filter (\(c,g) -> g /= 0) p

deriv' :: Polinomio -> Polinomio
deriv' p = [ (c * fromIntegral g, g - 1) | (c,g) <- p, g /= 0]

-- Alínea e)

calcula :: Float -> Polinomio -> Float
calcula x p = foldl (\acc (c,g) -> acc + c * x ^ g) 0 p

-- Alínea f)

simp :: Polinomio -> Polinomio
simp = filter (\(c,g) -> c /= 0) 

-- Alínea g)

mult :: Monomio -> Polinomio -> Polinomio
mult (cm,gm) = map (\(c,g) -> (cm*c,gm+g))

-- Alínea h)

ordena :: Polinomio -> Polinomio
ordena = sortOn snd

-- Alínea i)

normaliza :: Polinomio -> Polinomio
normaliza p = map (foldl (\(cacc,gacc) (c,g) -> (cacc+c,g)) (0,0)) $ groupBy (\m1 m2 -> snd m1 == snd m2) $ ordena p

-- Alínea j)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = foldl (\acc m -> adiciona m acc) p1 p2
    where adiciona :: Monomio -> Polinomio -> Polinomio
          adiciona m [] = [m]
          adiciona (cm,gm) ((c,g):t) = if gm == g then (cm+c,g) : t else (c,g) : adiciona (cm,gm) t

-- Alínea k)

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl (\acc m -> soma (mult m p2) acc) [] p1

-- Alínea l)

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = null (simp (soma p1 (mult (-1,0) p2)))

-- Exercício 3

type Mat a = [[a]]

-- Alínea a)

dimOK :: Mat a -> Bool
dimOK = (== 1) . length . nub . map length

-- Alínea b)

dimMat :: Mat a -> (Int, Int)
dimMat [] = (0,0)
dimMat m = (length m, length (head m))

-- Alínea c)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith (zipWith (+))

-- Alínea d)

transpose' :: Mat a -> Mat a
transpose' m = [ map (!! i) m |  i <- [0..c-1]]
    where (l,c) = dimMat m

-- Alínea e)

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = map (\l -> [ sum (zipWith (*) l [ x !! i | x <- m2 ]) | i <- [0..c-1] ]) m1
    where (l,_) = dimMat m1
          (_,c) = dimMat m2

-- Alínea f)

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat = zipWith . zipWith

-- Alínea g)

triSup :: (Num a, Eq a) => Mat a -> Bool
triSup m = and [ all (== 0) [ m !! i !! j | j <- [0..i-1] ] | i <- [0..length m - 1]]

-- Alínea h)

rotateLeft :: Mat a -> Mat a
rotateLeft m = [ map (!! i) m | i <- [c-1,c-2..0]] 
    where (l,c) = dimMat m