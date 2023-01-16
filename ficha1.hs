module Ficha1 where

import Data.Char

-- Exercício 1

-- Alínea a)

perimetro :: Float -> Float
perimetro raio = raio * pi * 2

-- Alínea b)

dist :: (Double, Double) -> (Double, Double) -> Double
dist (xa, ya) (xb, yb) = sqrt ((xa - xb) ^ 2 + (ya - yb) ^ 2)

-- Alínea c)

primUlt :: [a] -> (a,a)
primUlt lista = (head lista, last lista)

-- Alínea d)

multiplo :: Int -> Int -> Bool
multiplo m n | m > n = mod m n == 0
             | otherwise = mod n m == 0 

-- Alínea e)

truncaImpar :: [a] -> [a]
truncaImpar lista = if not (multiplo (length lista) 2)
                    then tail lista 
                    else lista 

-- Alínea f)

max2 :: Int -> Int -> Int
max2 a b = if a > b 
           then a
           else b

-- Alínea g)

max3 :: Int -> Int -> Int
max3 a b c = if max a b == a
             then max2 a c 
             else max2 b c 

-- Exercício 2

-- Alínea a)

nRaizes :: (Double,Double,Double) -> Int
nRaizes (a,b,c)
    | delta > 0 = 2
    | delta == 0 = 1
    | delta < 0 = 0
    where delta = b ^ 2 - 4 * a * c

-- Alínea b)

raizes :: (Double,Double,Double) -> [Double]
raizes (a,b,c)
    | n == 2 = [(-b + sqrt (b ^ 2 - 4 * a * c))/(2*a),(-b - sqrt (b ^ 2 - 4 * a * c))/(2*a)]
    | n == 1 = [(-b + sqrt (b ^ 2 - 4 * a * c))/(2*a)]
    | n == 0 = []
    where n = nRaizes (a,b,c)

-- Exercício 3

type Hora = (Int,Int)

-- Alínea a)

horaValida :: Hora -> Bool
horaValida (h,m) = 0 <= h && h <= 23 &&
                   0 <= m && m <= 59

-- Alínea b)

horaDepoisDe :: Hora -> Hora -> Bool
horaDepoisDe (h1,m1) (h2,m2) = h1 > h2 || (h1 == h2 && m1 > m2)

-- Alínea c)

horaParaMinutos :: Hora -> Int
horaParaMinutos (h,m) = 60 * h + m

-- Alínea d)

minutosParaHora :: Int -> Hora
minutosParaHora m = divMod m 60

-- Alínea e)

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (h1,m1) (h2,m2) = (h1 - h2) * 60 + (m1 - m2)

-- Alínea f)

adicionaMinutos :: Hora -> Int -> Hora
adicionaMinutos (h,m) n = (h + hr,mr)
    where (hr,mr) = minutosParaHora (m + n)

-- Exercício 4

data Hora = H Int Int deriving (Show,Eq)

-- Alínea a)

horaValida :: Hora -> Bool
horaValida (H h m) = 0 <= h && h <= 23 &&
                     0 <= m && m <= 59

-- Alínea b)

horaDepoisDe :: Hora -> Hora -> Bool
horaDepoisDe (H h1 m1) (H h2 m2) = h1 > h2 || (h1 == h2 && m1 > m2)

-- Alínea c)

horaParaMinutos :: Hora -> Int
horaParaMinutos (H h m) = 60 * h + m

-- Alínea d)

minutosParaHora :: Int -> Hora
minutosParaHora m = H a b
    where (a,b) = divMod m 60

-- Alínea e)

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (H h1 m1) (H h2 m2) = (h1 - h2) * 60 + (m1 - m2)

-- Alínea f)

adicionaMinutos :: Hora -> Int -> Hora
adicionaMinutos (H h m) n = H (h + hr) mr
    where (H hr mr) = minutosParaHora (m + n)

-- Exercícios 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

-- Alínea a)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

-- Alínea b)

stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

-- Alínea c)

safe :: Semaforo -> Semaforo -> Bool
safe Vermelho _ = True
safe _ Vermelho = True
safe _ _ = False

-- Exercício 6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

-- Alínea a)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

-- Alínea b)

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

-- Alínea c)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x ^ 2 + y ^ 2)
raio (Polar r a) = r

-- Alínea d)

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar r a) = a

-- Alínea e)

dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt ((x' - x) ^ 2 + (y' - y) ^ 2)
    where x = posx p1
          y = posy p1
          x' = posx p2
          y' = posy p2

-- Exercício 7

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

-- Alínea a)

poligono :: Figura -> Bool
poligono (Circulo _ _ ) = False
poligono (Retangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2
poligono (Triangulo p1 p2 p3) = posx p1 /= posx p2 ||
                                posx p2 /= posx p3 ||
                                posx p1 /= posx p3
                                &&
                                posy p1 /= posy p2 ||
                                posy p2 /= posy p3 ||
                                posy p1 /= posy p3

-- Alínea b)

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo p1 p2) = [p1, Cartesiano (posx p1) (posy p2), p2, Cartesiano (posx p2) (posy p1)]
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]

-- Alínea c)

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Retangulo p1 p2) = abs (posx p2 - posx p1) * abs (posy p2 - posy p1)
area (Circulo _ r) = pi * (r ^ 2)

-- Alínea d)

perimetro :: Figura -> Double
perimetro (Circulo _ r) = 2 * pi * r
perimetro (Retangulo p1 p2) = abs (posx p2 - posx p1) * 2 + abs (posy p2 - posy p1) * 2
perimetro (Triangulo p1 p2 p3) = dist p1 p2 + dist p2 p3 + dist p1 p3

-- Exercício 8

-- Alínea a)

isLower :: Char -> Bool
isLower c = ord c >= ord 'a' && ord c <= ord 'z'

-- Alínea b)

isDigit :: Char -> Bool
isDigit d = ord ch >= ord '0' && ord ch <= ord '9'

-- Alínea c)

isAlpha :: Char -> Bool
isAlpha ch = isLower ch || isUpper ch
    where isUpper ch = ord ch >= ord 'A' && ord ch <= ord 'Z'

-- Alínea d)

toUpper :: Char -> Char
toUpper ch = if isLower ch then chr (ord ch - 32) else ch

-- Alínea e)

intToDigit :: Int -> Char
intToDigit n = chr (n + 48)

-- Alínea f)

digitToInt :: Char -> Int 
digitToInt ch = ord ch - 48