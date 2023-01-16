module Ficha6 where

-- Exercício 1

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

-- Alínea a)

altura :: BTree a -> Int
altura Empty = 0
altura (Node _ l r) = 1 + max (altura l) (altura r)

-- Alínea b)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ l r) = 1 + contaNodos l + contaNodos r

-- Alínea c)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ l r) = folhas l + folhas r

-- Alínea d)

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune x (Node e l r) = Node e (prune (x - 1) l) (prune (x - 1) r)

-- Alínea e)

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node e _ _) = [e]
path (h:t) (Node e l r) = e : path t (if h then r else l)  

-- Alínea f)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node e l r) = Node e (mirror r) (mirror l)

-- Alínea g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node e l r) (Node e' l' r') = Node (f e e') (zipWithBT f l l') (zipWithBT f r r')
zipWithBT _ _ _ = Empty

-- Alínea h)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) l r) = (Node a unzipL1 unzipR1, Node b unzipL2 unzipR2, Node c unzipL3 unzipR3)
    where (unzipL1,unzipL2,unzipL3) = unzipBT l
          (unzipR1,unzipR2,unzipR3) = unzipBT r

-- Exercício 2

-- Alínea a)

minimo :: (Ord a) => BTree a -> a
minimo (Node e Empty _) = e
minimo (Node e l r) = minimo l

-- Alínea b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty r) = r
semMinimo (Node e l r) = Node e (semMinimo l) r

-- Alínea c)

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node e Empty r) = (e,r)
minSmin (Node e l r) = (a,Node e b r)
    where (a,b) = minSmin l

-- Alínea d)

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node e l r) 
    | x < e = Node e (remove x l) r
    | x > e = Node e l (remove x r)
    | otherwise = case r of Empty -> l
                            _ -> let (g,h) = minSmin r in Node g l h

-- Exercício 3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show

type Turma = BTree Aluno

-- Alíena a)

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) l r) = n == num || inscNum n (if n < num then l else r)

-- Alínea b)

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nom,_,_) l r) = n == nom || inscNome n l || inscNome n r

-- Alínea c)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nom,TE,_) l r) = trabEst l ++ [(num,nom)] ++ trabEst r
trabEst (Node _ l r) = trabEst l ++ trabEst r

-- Alínea d)

nota :: Numero -> Turma -> Maybe Classificacao
nota n (Node (num,_,_,clas) l r) 
    | n == num = Just clas
    | n < num = nota n l
    | otherwise = nota n r
nota _ _ = Nothing

-- Alínea e)

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas turma = sumFaltas turma / numAlunos turma * 100
    where sumFaltas :: Turma -> Float
          sumFaltas Empty = 0
          sumFaltas (Node (_,_,_,Faltou) l r) = 1 + sumFaltas l + sumFaltas r
          sumFaltas (Node _ l r) = sumFaltas l + sumFaltas r
          numAlunos = fromIntegral . contaNodos

-- Alínea f)

mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov turma = uncurry (/) (sumNumNotas turma)
    where sumNumNotas :: Turma -> (Float, Float)
          sumNumNotas Empty = (0,0)
          sumNumNotas (Node (_,_,_,Aprov nota) l r) = addPairs (fromIntegral nota, 1) (addPairs (sumNumNotas l) (sumNumNotas r))
          sumNumNotas (Node _ l r) = addPairs (sumNumNotas l) (sumNumNotas r)
          addPairs (a,b) (c,d) = (a+c,b+d)

-- Alínea g)

aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = uncurry (/) (sumAprovAv turma)
          
sumAprovAv :: Turma -> (Float, Float)
sumAprovAv Empty = (0,0)
sumAprovAv (Node (_,_,_,clas) l r) = case clas of Aprov nota -> (ap+1,av+1) 
                                                  Rep -> (ap,av+1)
                                                  _ -> (ap,av)
    where (ap,av) = addPairs (sumAprovAv l) (sumAprovAv r)
          addPairs (a,b) (c,d) = (a+c,b+d)