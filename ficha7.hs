module Ficha7 where

-- Exercício 1

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

-- Alínea a)

calcula :: ExpInt -> Int
calcula (Const num) = num
calcula (Simetrico exp) = (- calcula exp)
calcula (Mais a b) = calcula a + calcula b
calcula (Menos a b) = calcula a - calcula b
calcula (Mult a b) = calcula a * calcula b

-- Alínea b)

infixa :: ExpInt -> String
infixa (Const num) = show num
infixa (Simetrico exp) = "(-(" ++ infixa exp ++ "))"
infixa (Mais a b) = '(':infixa a ++ " + " ++ infixa b ++ ")"
infixa (Menos a b) = '(':infixa a ++ " - " ++ infixa b ++ ")"
infixa (Mult a b) = '(':infixa a ++ " * " ++ infixa b ++ ")"

-- Alínea c)

posfixa :: ExpInt -> String
posfixa (Const num) = show num
posfixa (Simetrico exp) = posfixa exp ++ " (-) "
posfixa (Mais a b) = posfixa a ++ " " ++ posfixa b ++ " + "
posfixa (Menos a b) = posfixa a ++ " " ++ posfixa b ++ " - "
posfixa (Mult a b) = posfixa a ++ " " ++ posfixa b ++ " * "

-- Exercício 2

data RTree a = R a [RTree a]

-- Alínea a)

soma :: Num a => RTree a -> a
soma (R e []) = e
soma (R e es) = e + sum (map soma es)

-- Alínea b)

altura :: RTree a -> Int
altura (R e []) = 1
altura (R e es) = 1 + maximum (map altura es)

-- Alínea c)

prune :: Int -> RTree a -> RTree a
prune 0 (R e es) = R e []
prune n (R e es) = R e (map (prune (n - 1)) es)

-- Alínea d)

mirror :: RTree a -> RTree a
mirror (R e es) = R e (map mirror (reverse es))

-- Alínea e)

postorder :: RTree a -> [a]
postorder (R e []) = [e]
postorder (R e es) = concatMap postorder es ++ [e]

-- Exercício 3

data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)

-- Alínea a)

ltSum :: Num a => LTree a -> a
ltSum (Tip n) = n
ltSum (Fork a b) = ltSum a + ltSum b 

-- Alínea b)

listaLT :: LTree a -> [a]
listaLT (Tip n) = [n]
listaLT (Fork a b) = listaLT a ++ listaLT b

-- Alínea c)

ltHeight :: LTree a -> Int
ltHeight (Tip _) = 0
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b)

-- Exercício 4

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

-- Alínea a)

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf n) = (Empty, Tip n)
splitFTree (No a l r) = (Node a lb rb, Fork ll rl)
    where (lb, ll) = splitFTree l
          (rb, rl) = splitFTree r

-- Alínea b)

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip n) = Just (Leaf n)
joinTrees (Node e l r) (Fork a b) =
    case (joinTrees l a, joinTrees r b) of (Just x, Just y) -> Just (No e x y)
                                           _ -> Nothing
joinTrees _ _ = Nothing