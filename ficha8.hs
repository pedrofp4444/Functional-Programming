module Ficha8 where

-- Exercício 1

data Frac = F Integer Integer

-- Alínea a)

normaliza :: Frac -> Frac
normaliza (F a b)
    | b < 0 = normaliza $ F (-a) (-b)
    | otherwise = 
        let d = mdc a b in
        F (a `div` d) (b `div` d)

mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc 0 y = y
mdc x y = mdc y (x `mod` y)

-- Alínea b)

instance Eq Frac where
    (==) :: Frac -> Frac -> Bool
    f1 == f2 = a1 == a2 && b1 == b2
        where F a1 b1 = normaliza f1
              F a2 b2 = normaliza f2

-- Alínea c)

instance Ord Frac where
    (<=) :: Frac -> Frac -> Bool
    f1 <= f2 = a1 * b2 <= a2 * b1
        where F a1 b1 = normaliza f1
              F a2 b2 = normaliza f2

-- Alínea d)

instance Show Frac where
    show :: Frac -> String
    show f = show a ++ "/" ++ show b
        where F a b = normaliza f

-- Alínea e)

instance Num Frac where
    (+) :: Frac -> Frac -> Frac
    (F a b) + (F c d) = normaliza $ F (a * d + b * c) (b * d)
    
    (-) :: Frac -> Frac -> Frac
    x - y = x + negate y

    (*) :: Frac -> Frac -> Frac
    (F a b) * (F c d) = normaliza $ F (a * c) (b * d)
    
    negate :: Frac -> Frac
    negate (F a b) = normaliza $ F (-a) b
    
    abs :: Frac -> Frac
    abs f = F (abs a) b
        where F a b = normaliza f
    
    signum :: Frac -> Frac
    signum f = F (signum a) 1
        where F a b = normaliza f
    
    fromInteger :: Integer -> Frac
    fromInteger x = F x 1

-- Alínea f)

maioresQueDobro :: Frac -> [Frac] -> [Frac]
maioresQueDobro = filter . (<) . (2 *)

-- Exercício 2

data Exp a = Const a
| Simetrico (Exp a)
| Mais (Exp a) (Exp a)
| Menos (Exp a) (Exp a)
| Mult (Exp a) (Exp a)

-- Alínea a)

instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(- " ++ show a ++ ")"
    show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Menos a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

-- Alínea b)

valueOf :: Num a => Exp a -> a
valueOf (Const a) = a
valueOf (Simetrico a) = negate $ valueOf a
valueOf (Mais a b) = valueOf a + valueOf b
valueOf (Menos a b) = valueOf a - valueOf b
valueOf (Mult a b) = valueOf a * valueOf b

instance (Num a, Eq a) => Eq (Exp a) where
    (==) :: (Num a, Eq a) => Exp a -> Exp a -> Bool
    a == b = valueOf a == valueOf b

-- Alínea c)

instance (Ord a, Num a) => Num (Exp a) where
    (+) :: Num a => Exp a -> Exp a -> Exp a
    x + y = Mais x y
    
    (-) :: Num a => Exp a -> Exp a -> Exp a
    x - y = Menos x y
    
    (*) :: Num a => Exp a -> Exp a -> Exp a
    x * y = Mult x y
    
    negate :: Num a => Exp a -> Exp a
    negate (Simetrico a) = a
    negate a = Simetrico a
    
    fromInteger :: Num a => Integer -> Exp a
    fromInteger x = Const (fromInteger x)
    
    abs :: (Ord a, Num a) => Exp a -> Exp a
    abs (Const a) = Const (abs a)
    abs (Simetrico a) = abs a
    abs op = if valueOf op < 0 
             then negate op
             else op
    
    signum :: Num a => Exp a -> Exp a
    signum a | valueOf a < 0 = Const (-1)
             | valueOf a == 0 = Const 0
             | otherwise = Const 1 

-- Exercício 3

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

-- Alínea a)

instance Ord Data where
    compare :: Data -> Data -> Ordering
    compare (D dia1 mes1 ano1) (D dia2 mes2 ano2) 
        | ano1 > ano2 || ano1 == ano2 && (mes1 > mes2 || mes1 == mes2 && dia1 > dia2) = GT
        | ano1 == ano2 && mes1 == mes2 && dia1 == dia2 = EQ
        | otherwise = LT

-- Alínea b)

instance Show Data where 
    show :: Data -> String
    show (D dia mes ano) = intercalate "/" $ map show [ano,mes,dia]

-- Alímea c)

ordena :: Extracto -> Extracto
ordena (Ext n l) = Ext n (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l)

-- Alínea d

instance Show Extracto where
    show :: Extracto -> String
    show ext = "Saldo anterior: " ++ show n ++
               "\n---------------------------------------" ++
               "\nData       Descricao" ++ replicate (desc_max - 9) ' ' ++ "Credito" ++ replicate (cred_max - 7) ' ' ++ "Debito" ++
               "\n---------------------------------------\n" ++
               unlines (map (\(dat,desc,mov) -> 
                    show dat ++ replicate (data_max - length (show dat)) ' ' 
                    ++ map toUpper desc ++ replicate (desc_max - length desc) ' ' 
                    ++ case mov of Credito quant -> show quant ++ replicate (cred_max - length (show quant)) ' '; Debito _ -> replicate cred_max ' '
                    ++ case mov of Debito quant -> show quant; Credito _ -> ""
               ) movs) ++
               "---------------------------------------" ++
               "\nSaldo actual: " ++ show (saldo ext)
        where (Ext n movs) = ordena ext
              data_max = 11
              desc_max = max (length "Descricao   ") (maximum $ map (\(_,desc,_) -> length desc) movs)
              cred_max = max (length "Credito   ") (maximum $ map (\(_,_,mov) -> case mov of Credito x -> length (show x); _ -> 0) movs)