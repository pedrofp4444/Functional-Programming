module Ficha2 where

import Data.Char

-- Exercício 1

-- Alínea a)

funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y ^ 2 + (funA ys)

-- Alínea b)

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if mod h 2 == 0 
             then h : funB t
             else funB t

-- Alínea c)

funC :: [a] -> [a]
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

-- Alínea d)

funD :: [a] -> [a]
funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t

-- Exercício 2

-- Alínea a)

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h * 2 : dobros t

-- Alínea b)

numOcorre :: Char -> String -> Int
numOcorre _ "" = 0
numOcorre c (h:t) = if c == h
                    then 1 + numOcorre c t
                    else numOcorre c t

-- Alínea c)

positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if h <= 0
                  then False
                  else positivos t

-- Alínea d)

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t)
    | h > 0 = h : soPos t
    | otherwise = soPos t

-- Alínea e)

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t)
    | h < 0 = h + somaNeg t
    | otherwise = somaNeg t

-- Alínea f)

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t)
    | length t < 3 = h:t
    | otherwise = tresUlt t

-- Alínea g) 

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((_,h2):t) = h2 : segundos t

-- Alínea h)

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x ((h1,_):t) = x == h1 || nosPrimeiros x t

-- Alínea i)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a+ra, b+rb, c+rc)
    where (ra,rb,rc) = sumTriplos t

-- Exercício 3

-- Alínea a)

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t)
    | isDigit h = h : soDigitos t
    | otherwise = soDigitos t

-- Alínea b)

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t)
    | isLower h = 1 + minusculas t
    | otherwise = minusculas t

-- Alínea c)

nums :: String -> [Int]
nums "" = []
nums (h:t)
    | isDigit h = digitToInt h : nums t
    | otherwise = nums t

-- Exercício 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- Alínea a)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((coeficiente,grau):t)
    | n == grau = 1 + conta n t
    | otherwise = conta n t

-- Alínea b)

grau :: Polinomio -> Int
grau [] = 0
grau ((c,g):t)
    | g > grau t = g
    | otherwise = grau t

-- Alínea c)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((c,g):t)
    | n == g = (c,g) : selgrau n t
    | otherwise = selgrau n t

-- Alínea d)

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,g):t)
    | g == 0 = deriv t
    | otherwise = (c * fromIntegral g,g-1) : deriv t

-- Alínea e)

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((c,g):t) = c * (x ^ g) + calcula x t

-- Alínea f)

simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,g):t)
    | c == 0 = simp t
    | otherwise = (c,g) : simp t

-- Alínea g)

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (cm,gm) ((c,g):t) = (cm * c, gm + g) : mult (cm,gm) t

-- Alínea h)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,g):t) = normalizaAux (c,g) (normaliza t)

normalizaAux :: Monomio -> Polinomio -> Polinomio
normalizaAux m [] = [m]
normalizaAux (cm,gm) ((c,g):t)
    | gm == g = (cm + c,g) : t
    | otherwise = (c,g) : normalizaAux (cm,gm) t

-- Alínea i)

soma :: Polinomio -> Polinomio -> Polinomio
soma p [] = p
soma [] p = p
soma ((c,g):t) p = somaAux (c,g) (soma t p)

somaAux :: Monomio -> Polinomio -> Polinomio
somaAux m [] = [m]
somaAux (cm,gm) ((c,g):t)
    | gm == g = (cm + c,g) : t
    | otherwise = (c,g) : somaAux (cm,gm) t

-- Alínea j)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (m:t) p = (mult m p) ++ produto t p

-- Alínea k)

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:t) = insere m (ordena t)

insere :: Monomio -> Polinomio -> Polinomio
insere m [] = [m]
insere (cm,gm) ((c,g):t)
    | g > gm = (cm,gm) : (c,g) : t
    | otherwise = (c,g) : insere (cm,gm) t

-- Alínea l)

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)